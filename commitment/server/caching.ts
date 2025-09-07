import { Subject } from "rxjs"
import { Mongo } from "meteor/mongo"
import { Meteor } from "meteor/meteor"

import { RepositoryData, SerializableRepoData } from "../imports/api/types"
import { deserializeRepoData, serializeRepoData } from "../imports/api/serialisation"

/**
 * COLLECTION OF REPOSITORY METHODS
 */
export interface ServerRepoData {
  _id?: string;
  url: string;
  createdAt: Date;
  data: SerializableRepoData;
}

const RepoCollection = new Mongo.Collection<ServerRepoData>("repoCollection")

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

    const d = await RepoCollection.findOneAsync({ url })

    if (!d) {
      throw new Meteor.Error("link-not-found", "Link not found")
    }

    return RepoCollection.removeAsync({ url })
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
    const ret = await RepoCollection.findOneAsync({ url })
    return ret !== null
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
    const bm = await RepoCollection.findOneAsync({ url })

    if (!bm) {
      throw new Meteor.Error("bookmark-not-found", "Bookmark not found");
    }

    return RepoCollection.updateAsync({ url }, { $set: { lastViewed: new Date() } })
  },

  /**
   * Gets the data for a specific repository.
   *
   * @method repoCollection.getRepoData
   * @param {string} url - The URL of the repository.
   * @returns {Promise<SerializableRepoData>} The repository data.
   * @throws {Meteor.Error} If the repository is not found.
   */
  async "repoCollection.getData"(url: string) {
    const repoData = await RepoCollection.findOneAsync({ url });
    if (!repoData) {
      throw new Meteor.Error("repository-not-found", "Repository not found")
    }
    return repoData.data;
  }
})

/**
 * Checks if a repository exists in the database.
 * @param url The repository URL to check.
 * @returns True if the repository exists, false otherwise.
 */
export const isInDatabase = async (url: string): Promise<boolean> => {
  const result = await RepoCollection.findOneAsync({ url });
  return result !== null;
}

/**
 * Tries to get repository data from the database.
 * @param url The repository URL.
 * @param notifier Subject to notify about the status.
 * @returns Promise that resolves to the repository data.
 */
export const tryFromDatabase = (
  url: string,
  notifier: Subject<string>
): Promise<RepositoryData> => new Promise((resolve, reject) => {
  notifier.next("Checking database for existing data...");
  Meteor.callAsync("repoCollection.getData", url)
    .then(d => {
      // TODO CHECK IF REPO DATA IS MOST UP TO DATE
      notifier.next("Found data in database!");
      resolve(deserializeRepoData(d));
    })
    .catch((_e: Error) => {
      reject(new Error("Data not found in database"))
    })
})

/**
 * Caches repository data in the database.
 * @param url The repository URL.
 * @param data The repository data to cache.
 */
export const cacheIntoDatabase = (url: string, data: RepositoryData): Promise<void> => 
  Meteor.callAsync("repoCollection.insertOrUpdateRepoData", 
    url, 
    serializeRepoData(data)
  )

