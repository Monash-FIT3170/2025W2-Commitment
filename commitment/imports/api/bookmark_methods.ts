import { Meteor } from 'meteor/meteor';
import { check } from 'meteor/check';
import { BookmarksCollection, Bookmark } from './bookmarks';

import { repoInDatabase } from "./call_repo"

Meteor.methods({
    /**
     * Inserts a new link into the LinksCollection.
     *
     * @method links.insert
     * @param {string} title - The title of the link.
     * @param {string} url - The URL of the link. Must start with 'http' or 'https'.
     * @param {string} userID - The ID of the user inserting the link.
     * @returns {Promise<string>} The ID of the newly inserted link document.
     * @throws {Meteor.Error} If the URL is invalid or does not start with 'http'.
     */
    async 'bookmarks.insertBookmark'(title: string, url: string, userID: string) {
        check(title, String);
        check(url, String);
        check(userID, String);

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
            userID
        };

        return await BookmarksCollection.insertAsync(newBookmark);
    },

    /**
     * Removes a link from the LinksCollection by its URL.
     *
     * @method links.remove
     * @param {string} url - The URL of the link to be removed.
     * @returns {Promise<number>} The number of documents removed (should be 1 if successful).
     * @throws {Meteor.Error} If no link with the given URL is found.
     */
    async 'bookmarks.removeBookmark'(url: string, userID: string) {
        check(url, String);
        
        const bm = await BookmarksCollection.findOneAsync({ url: url, userID: userID });
        
        if (!bm) {
            throw new Meteor.Error('link-not-found', 'Link not found');
        }

        const result = BookmarksCollection.removeAsync(bm._id);
        return result;
    },

    /**
     * Checks whether a link with the given URL exists in the LinksCollection.
     *
     * @method links.isBookmarked
     * @param {string} url - The URL to check.
     * @returns {Promise<boolean>} True if the URL is bookmarked, false otherwise.
     */
    async 'bookmarks.isBookmarked'(url: string, userID: string) {
        check(url, String);
        check(userID, String);
        
        const bm = await BookmarksCollection.findOneAsync({ url: url, userID: userID });

        // If the link exists and belongs to the user, return true
        return !!bm;
    },
});
