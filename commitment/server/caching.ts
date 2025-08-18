import { Subject } from 'rxjs';
import { Mongo } from 'meteor/mongo';
import {
  RepositoryData,
  SerializableRepoData,
  ServerRepoData,
} from './commitment_api/types';
import { fetchDataFrom } from './commitment_api/commitment';

/**
 * COLLECTION OF REPOSITORIES
 */
export const RepoCollection = new Mongo.Collection<ServerRepoData>('repoCollection');

Meteor.methods({
  /**
   * Upsert (Insert or Update) repository data into repoCollection.
   *
   * @param url The URL of the repository to insert or update.
   * @param data The repository data to be saved, which should be of type SerializableRepoData
   *                (or else we lose our data).
   *
   * @returns The result of the upsert operation, which includes the number of documents affected.
   * @throws Throws an error if the URL is invalid, not in the database, or not authorised.
   */
  async 'repoCollection.insertOrUpdateRepoData'(
    url: string,
    data: SerializableRepoData,
  ) {
    const updateDoc = {
      $set: {
        data,
        createdAt: new Date(),
      },
    };

    const result = await RepoCollection.upsertAsync(
      { url }, // filter to find existing doc
      updateDoc, // update operation
    );

    return result;
  },

  /**
   * Inserts a new link into the LinksCollection.
   *
   * @method links.insert
   *
   * @param {string} url The URL of the link. Must start with 'http' or 'https'.
   * @param {string} data the repo metadata to be saved
   *
   * @returns {Promise<string>} The ID of the newly inserted link document.
   * @throws {Meteor.Error} If the URL is invalid or does not start with 'http',
   *                            not in db or not authorised.
   */
  async 'repoCollection.insertRepoData'(url: string, data: RepositoryData) {
    const s: ServerRepoData = {
      url,
      createdAt: new Date(),
      data,
    };

    const result = await RepoCollection.insertAsync(s);
    return result;
  },

  /**
   * Removes a repo from the RepoCollection by its URL.
   *
   * @method links.remove
   *
   * @param {string} url The URL of the link to be removed.
   *
   * @returns {Promise<number>} The number of documents removed (should be 1 if successful).
   * @throws {Meteor.Error} If no link with the given URL is found or not authorised.
   */
  async 'repoCollection.removeRepo'(url: string) {
    if (!isInDatabase(url)) {
      throw new Meteor.Error(
        'not-in-database',
        'The repo must exist in the database to be removed',
      );
    }

    const d = await RepoCollection.findOneAsync({ url });

    if (!d) {
      throw new Meteor.Error('link-not-found', 'Link not found');
    }

    return RepoCollection.removeAsync(d._id);
  },

  /**
   * Checks whether a link with the given URL exists in the LinksCollection.
   *
   * @method links.isBookmarked
   *
   * @param {string} url The URL to check.
   *
   * @returns {Promise<boolean>} True if the URL is bookmarked, false otherwise.
   * @throws {Meteor.Error} If no link with the given URL is found or not authorised.
   */
  async 'repoCollection.exists'(url: string) {
    const ret = await RepoCollection.findOneAsync({ url });
    return ret !== null;
  },

  /**
   * Updates the lastViewed parameter of the bookmark.
   *
   * @method bookmarks.updateLastViewed
   *
   * @param {string} url The URL of the repository to update.
   *
   * @returns {Promise<number>} The number of documents updated (should be 1 if successful).
   * @throws {Meteor.Error} If the URL is invalid, bookmark not found, or not authorised.
   */
  async 'repoCollection.updateLastViewed'(url: string) {
    const bm = await RepoCollection.findOneAsync({ url });
    if (!bm) {
      throw new Meteor.Error('bookmark-not-found', 'Bookmark not found');
    }

    return RepoCollection.updateAsync(bm._id, {
      $set: { lastViewed: new Date() },
    });
  },

  /**
   * Get repository data by URL - added method by Milni in order to actually retrieve saved repo data
   *
   * @method repoCollection.getRepoData
   *
   * @param {string} url - The URL of the repository.
   *
   * @returns {Promise<SerializableRepoData>} The repository data.
   * @throws {Meteor.Error} If the repository data is not found or not authorised.
   *
   */
  async 'repoCollection.getData'(url: string) {
    const repoData = await RepoCollection.findOneAsync({ url });
    if (!repoData) {
      throw new Meteor.Error('not-found', 'Repo data not found');
    }

    return repoData.data;
  },
});

/**
 * Method to cache fetched data into the database.
 *
 * @param url URL of the repository to cache.
 * @param data The repo data to be cached, should be of SerializableRepoData type.
 *
 * @returns {Promise<boolean>} A promise that resolves to true if the data was successfully cached,
 *                              or false otherwise.
 * @throws {Error} If there is an error during the caching process.
 */
const cacheIntoDatabase = async (
  url: string,
  data: SerializableRepoData,
): Promise<boolean> =>
  // Use Meteor.call to insert or update the repo data in the database
  new Promise((resolve, reject) => {
    Meteor.call(
      'repoCollection.insertOrUpdateRepoData',
      url,
      data,
      (err: any, res: boolean | PromiseLike<boolean>) => {
        if (err) {
          console.error('Error inserting repo data:', err);
          reject(err);
        } else {
          resolve(res);
        }
      },
    );
  });

/**
 * Method to check if a repository exists in the database.
 *
 * @param url URL of the repository to check.
 * @returns {Promise<boolean>} A promise that resolves to true if the repository exists in the database,
 *                              or false otherwise.
 * @throws {Error} If there is an error during the check.
 */
export const isInDatabase = async (url: string): Promise<boolean> => Meteor.call('repoCollection.exists', url);

/**
 * Tries to retrieve repository data from the database.
 *
 * @param url URL of the repository to search for.
 * @param notifier Subject to notify about the status of the search.
 *
 * @returns {Promise<SerializableRepoData>} A promise that resolves to the repository data if found.
 * @throws {Error} If no data is found in the database.
 */
const tryFromDatabase = async (
  url: string,
  notifier: Subject<string>,
): Promise<SerializableRepoData> => {
  // try and get it from database
  notifier.next('Searching database for your repo...');
  const data = await RepoCollection.findOneAsync({ url });
  if (!data) {
    notifier.next('No data found in DB.');
    return Promise.reject('No data found');
  }
  notifier.next('Found your repo!');
  return data;
};

/**
 * Fetches repository data from an external source.
 *
 * @param url URL of the repository to fetch data from.
 * @param notifier Subject to notify about the status of the fetch operation.
 *
 * @returns {Promise<SerializableRepoData>} A promise that resolves to the fetched repository data.
 * @throws {Error} If there is an error during the fetch operation.
 */
export const getRepoData = async (
  url: string,
  notifier: Subject<string>,
): Promise<SerializableRepoData> => {
  try {
    // to update back to this line of code:
    const data = await tryFromDatabase(url, notifier);
    // delete these two lines later (need to update the URL being tested):
    // const data = await fetchDataFrom(url, notifier);
    // console.log("fetched data in getRepoData and checking type of allCommits", typeof data.allCommits);
    // await cacheIntoDatabase(url, data);

    return data;
  } catch (e) {
    const data = await fetchDataFrom(url, notifier);
    await cacheIntoDatabase(url, data);
    // const savedData = await RepoCollection.findOneAsync({ url });
    return data;
  }
};
