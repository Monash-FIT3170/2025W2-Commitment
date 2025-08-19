import { Meteor } from 'meteor/meteor';
import { check } from 'meteor/check';
import { LinksCollection, Link } from './links';

import { repoInDatabase } from './call_repo';

Meteor.methods({
  /**
     * Inserts a new link into the LinksCollection.
     *
     * @method links.insert
     * @param {string} title - The title of the link.
     * @param {string} url - The URL of the link. Must start with 'http' or 'https'.
     * @returns {Promise<string>} The ID of the newly inserted link document.
     * @throws {Meteor.Error} If the URL is invalid or does not start with 'http'.
     */
  async 'links.insert'(title: string, url: string) {
    check(title, String);
    check(url, String);

    if (!url.startsWith('http')) {
      throw new Meteor.Error('invalid-url', 'URL must be valid and start with http or https');
    }

    const inDatabase = repoInDatabase(url);
    if (!inDatabase) Promise.reject(new Error(`URL does not exist inside database: ${url}`));

    const newLink: Link = {
      title,
      url,
      createdAt: new Date(),
    };

    return LinksCollection.insertAsync(newLink);
  },

  /**
     * Removes a link from the LinksCollection by its URL.
     *
     * @method links.remove
     * @param {string} url - The URL of the link to be removed.
     * @returns {Promise<number>} The number of documents removed (should be 1 if successful).
     * @throws {Meteor.Error} If no link with the given URL is found.
     */
  async 'links.remove'(url: string) {
    check(url, String);

    const link = await LinksCollection.findOneAsync({ url });

    if (!link) {
      throw new Meteor.Error('link-not-found', 'Link not found');
    }

    const result = LinksCollection.removeAsync(link._id);
    return result;
  },

  /**
     * Checks whether a link with the given URL exists in the LinksCollection.
     *
     * @method links.isBookmarked
     * @param {string} url - The URL to check.
     * @returns {Promise<boolean>} True if the URL is bookmarked, false otherwise.
     */
  async 'links.isBookmarked'(url: string) {
    check(url, String);
    const link = await LinksCollection.findOneAsync({ url });
    return !!link;
  },
});
