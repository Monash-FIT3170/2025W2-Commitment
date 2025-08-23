import { Subject } from "rxjs";
import { Mongo } from "meteor/mongo";

import { RepositoryData, SerializableRepoData } from "../imports/api/types";
import { deserializeRepoData, serializeRepoData } from "/imports/api/serialisation"

/**
 * COLLECTION OF REPOSITORY METHODS
 */
export interface ServerRepoData {
  _id?: string;
  url: string;
  createdAt: Date;
  data: SerializableRepoData;
}

const RepoCollection = new Mongo.Collection<ServerRepoData>("repoCollection");

Meteor.methods({
  /**
   * Inserts a new link into the LinksCollection.
   *
   * @method repoCollection.insertOrUpdateRepoData
   * @param {string} url - The URL of the link. Must start with 'http' or 'https'.
   * @param {SerializableRepoData} data - the repo metadata to be saved
   * @returns {Promise<string>} The ID of the newly inserted link document.
   * @throws {Meteor.Error} If the URL is invalid or does not start with 'http', not in db or not authorised.
   */
  async "repoCollection.insertOrUpdateRepoData"(url: string, data: SerializableRepoData) {
    const s: ServerRepoData = {
      url,
      createdAt: new Date(),
      data: data,
    };
    return await RepoCollection.upsertAsync(
      { url }, // filter to find existing doc
      s // update operation
    );
  },

  /**
   * Removes a repo from the RepoCollection by its URL.
   *
   * @method repoCollection.removeRepo
   * @param {string} url - The URL of the link to be removed.
   * @returns {Promise<number>} The number of documents removed (should be 1 if successful).
   * @throws {Meteor.Error} If no link with the given URL is found or not authorised.
   */
  async "repoCollection.removeRepo"(url: string) {
    if (!isInDatabase(url))
      throw new Meteor.Error(
        "not-in-database",
        "The repo must exist in the database to be removed"
      );

    const d = await RepoCollection.findOneAsync({ url });

    if (!d) {
      throw new Meteor.Error("link-not-found", "Link not found");
    }

    return RepoCollection.removeAsync({ url });
  },

  /**
   * Checks whether a link with the given URL exists in the LinksCollection.
   *
   * @method repoCollection.exists
   * @param {string} url - The URL to check.
   * @returns {Promise<boolean>} True if the URL is bookmarked, false otherwise.
   * @throws {Meteor.Error} If no link with the given URL is found or not authorised.
   */
  async "repoCollection.exists"(url: string) {
    const ret = await RepoCollection.findOneAsync({ url });
    return ret !== null;
  },

  /**
   * Checks whether a link with the given URL exists in the LinksCollection.
   *
   * @method repoCollection.allUrls
   * @returns {string[]} all urls existing in the database
   */
  async "repoCollection.allUrls"() {
    return RepoCollection.find().fetch().map(d => d.url)
  },

  /**
   * Updates the lastViewed parameter of the bookmark.
   *
   * @method repoCollection.updateLastViewed
   * @param {string} url - The URL of the bookmark to update.
   * @returns {Promise<number>} The number of documents updated (should be 1 if successful).
   * @throws {Meteor.Error} If the URL is invalid, bookmark not found, or not authorised.
   */
  async "repoCollection.updateLastViewed"(url: string) {
    const bm = await RepoCollection.findOneAsync({ url });

    if (!bm) {
      throw new Meteor.Error("bookmark-not-found", "Bookmark not found");
    }

    return RepoCollection.updateAsync({ url }, { $set: { lastViewed: new Date() } });
  },

  /**
   * Get repository data by URL - added method by Milni in order to actually retrieve saved repo data
   *
   * @method repoCollection.getRepoData
   * @param {string} url - The URL of the repository.
   * @returns {Promise<SerializableRepoData>} The repository data.
   * @throws {Meteor.Error} If the repository data is not found or not authorised.
   *
   */
  async "repoCollection.getData"(url: string) {
    const repoData = await RepoCollection.findOneAsync({ url });

    if (!repoData) {
      throw new Meteor.Error("not-found", "Repo data not found");
    }

    return repoData.data;
  },
});

/**
 * Method to cache fetched data into the database.
 * Use Meteor.call to insert or update the repo data in the database
 *
 * @param url URL of the repository to cache.
 * @param data The repo data to be cached, should be of RepositoryData type.
 *
 * @returns {Promise<boolean>} A promise that resolves to true if the data was successfully cached,
 *                              or false otherwise.
 * @throws {Error} If there is an error during the caching process.
 */
export const cacheIntoDatabase = async (url: string, data: RepositoryData): Promise<boolean> =>
  new Promise((resolve, _reject) => {
    Meteor.call(
      "repoCollection.insertOrUpdateRepoData",
      url,
      serializeRepoData(data),
      (err: any, _res: any) => {
        if (err) resolve(false);
        else resolve(true);
      }
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
export const isInDatabase = async (url: string): Promise<boolean> =>
  Meteor.call("repoCollection.exists", url);

/**
 * Tries to retrieve repository data from the database.
 *
 * @param url URL of the repository to search for.
 * @param notifier Subject to notify about the status of the search.
 *
 * @returns {Promise<RepositoryData>} A promise that resolves to the repository data if found.
 * @throws {Error} If no data is found in the database.
 */
export const tryFromDatabase = async (
  url: string,
  notifier: Subject<string>
): Promise<RepositoryData> =>
  new Promise((resolve, reject) => {
    // try and get it from database, catching an error as a failed fetch
    notifier.next("Searching database for your repo...");
    Meteor.call(
      "repoCollection.getData",
      url,
      (err: Error, res: SerializableRepoData) => {
        if (err) {
          const s = "Couldn't find your repo in the database";
          notifier.next(s);
          return reject(s);
        }

        notifier.next("Found your repo in the database!");
        return resolve(deserializeRepoData(res));
      }
    );
  });
