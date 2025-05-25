import { Meteor } from 'meteor/meteor';
import { check } from 'meteor/check';
import { BookmarksCollection, Bookmark } from './bookmarks';

import { repoInDatabase } from "./call_repo"
import { last } from 'rxjs';

Meteor.methods({
    /**
     * Inserts a new link into the LinksCollection.
     *
     * @method links.insert
     * @param {string} title - The title of the link.
     * @param {string} url - The URL of the link. Must start with 'http' or 'https'.
     * @returns {Promise<string>} The ID of the newly inserted link document.
     * @throws {Meteor.Error} If the URL is invalid or does not start with 'http', not in db or not authorised.
     */
    async 'bookmarks.insertBookmark'(title: string, url: string) {
        check(title, String);
        check(url, String);

        if (!this.userId) {
            throw new Meteor.Error('not-authorized', 'You must be logged in to add a bookmark');
        }

        if (!url.startsWith('http')) {
            throw new Meteor.Error('invalid-url', 'URL must be valid and start with http or https');
        }

        const inDatabase = await repoInDatabase(url);
        if (!inDatabase) {
            throw new Meteor.Error('not-in-database', `URL does not exist inside database: ${url}`);
        }

        const newBookmark: Bookmark = {
            title,
            url,
            createdAt: new Date(),
            userID: this.userId,
            lastViewed:new Date()
        };

        return await BookmarksCollection.insertAsync(newBookmark);
    },

    /**
     * Removes a link from the LinksCollection by its URL.
     *
     * @method links.remove
     * @param {string} url - The URL of the link to be removed.
     * @returns {Promise<number>} The number of documents removed (should be 1 if successful).
     * @throws {Meteor.Error} If no link with the given URL is found or not authorised.
     */
    async 'bookmarks.removeBookmark'(url: string) {
        check(url, String);

        if (!this.userId) {
            throw new Meteor.Error('not-authorized', 'You must be logged in to remove a bookmark.');
        }

        const bm = await BookmarksCollection.findOneAsync({ url, userID: this.userId });

        if (!bm) {
            throw new Meteor.Error('link-not-found', 'Link not found');
        }

        return BookmarksCollection.removeAsync(bm._id);
    },

    /**
     * Checks whether a link with the given URL exists in the LinksCollection.
     *
     * @method links.isBookmarked
     * @param {string} url - The URL to check.
     * @returns {Promise<boolean>} True if the URL is bookmarked, false otherwise.
     * @throws {Meteor.Error} If no link with the given URL is found or not authorised.
     */
    async 'bookmarks.isBookmarked'(url: string) {
        check(url, String);

        if (!this.userId) {
            throw new Meteor.Error('not-authorized', 'You must be logged in to check bookmarks.');
        }

        const bm = await BookmarksCollection.findOneAsync({ url, userID: this.userId });
        return !!bm;
    },

        /**
     * Updates the lastViewed parameter of the bookmark.
     *
     * @method bookmarks.updateLastViewed
     * @param {string} url - The URL of the bookmark to update.
     * @returns {Promise<number>} The number of documents updated (should be 1 if successful).
     * @throws {Meteor.Error} If the URL is invalid, bookmark not found, or not authorised.
     */
         async 'bookmarks.updateLastViewed'(url: string) {
            check(url, String);
    
            if (!this.userId) {
                throw new Meteor.Error('not-authorized', 'You must be logged in to update last viewed time.');
            }
    
            const bm = await BookmarksCollection.findOneAsync({ url, userID: this.userId });
    
            if (!bm) {
                throw new Meteor.Error('bookmark-not-found', 'Bookmark not found');
            }
    
            return BookmarksCollection.updateAsync(bm._id, { $set: { lastViewed: new Date() } });
        },

    /**
     * Retrieves all bookmarks for the current user.
     *
     * @method bookmarks.getAllBookmarks
     * @returns {Promise<Bookmark[]>} An array of bookmarks for the current user.
     * @throws {Meteor.Error} If not authorised.
     */
    async 'bookmarks.getAllBookmarks'() {
        if (!this.userId) {
            throw new Meteor.Error('not-authorized', 'You must be logged in to view bookmarks.');
        }

        return await BookmarksCollection.findAsync({ userID: this.userId }, { sort: { createdAt: -1 } });
    }
});
