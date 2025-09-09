import { Subject } from "rxjs"
import { Mongo } from "meteor/mongo"
import { Meteor } from "meteor/meteor"

import { RepositoryData, SerializableRepoData } from "../imports/api/types"
import { deserializeRepoData, serializeRepoData } from "../imports/api/serialisation"
import { meteorCallAsync } from "../imports/api/meteor_interface"

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
   * Inserts a new repo into the repoCollection.
   *
   * @method repoCollection.insertOrUpdateRepoData
   * @param {string} url - The URL of the repo. Must start with 'http' or 'https'.
   * @param {SerializableRepoData} data - the repo metadata to be saved
   * @returns {Promise<boolean>} whether the repo was successfully inserted or not
   * @throws {Meteor.Error} If the URL is invalid or does not start with 'http', not in db or not authorised.
   */
  async "repoCollection.insertOrUpdateRepoData"(url: string, data: SerializableRepoData): Promise<boolean> {
    const s: ServerRepoData = {
      url,
      createdAt: new Date(),
      data: data,
    };

    return await RepoCollection.upsertAsync(
        { url }, // filter to find existing doc
        s        // update operation
      )
      .then((_d: any) => true)
      .catch((_e: Error) => false);
  },

  /**
   * Gets the data for a specific repository.
   *
   * @method repoCollection.getData
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
  },

  /**
   * Removes a repo from the RepoCollection by its URL.
   *
   * @method repoCollection.removeRepo
   * @param {string} url - The URL of the link to be removed.
   * @returns {Promise<boolean>} whether the removal was successful
   * @throws {Meteor.Error} If no link with the given URL is found or not authorised.
   */
  async "repoCollection.removeRepo"(url: string): Promise<boolean> {
    const existing = await RepoCollection.findOneAsync({ url })
    if (!existing) {
      throw new Meteor.Error(
        "not-in-database", 
        "The repo must exist in the database to be removed"
      )
    }

    return RepoCollection.removeAsync({ url })
      .then((d: number) => d > 0)
      .catch((e: Error) => false)
  },

  /**
   * Checks whether a link with the given URL exists in the repoCollection.
   *
   * @method repoCollection.exists
   * @param {string} url - The URL to check.
   * @returns {Promise<boolean>} True if the URL is bookmarked, false otherwise.
   * @throws {Meteor.Error} If no link with the given URL is found or not authorised.
   */
  async "repoCollection.exists"(url: string): Promise<boolean> {
    return null !== await RepoCollection.findOneAsync({ url })
  },

  /**
   * Checks whether a link with the given URL exists in the repoCollection.
   *
   * @method repoCollection.allUrls
   * @returns {Promise<string[]>} all urls existing in the database
   */
  async "repoCollection.allUrls"(): Promise<string[]> {
    return await RepoCollection.find().fetch()
      .then((d: ServerRepoData[]) => d.map((d: ServerRepoData) => d.url))
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

    return await RepoCollection.updateAsync({ url }, { $set: { lastViewed: new Date() } })
  }
})

/**
 * Checks if a repository exists in the database.
 * @param url The repository URL to check.
 * @returns True if the repository exists, false otherwise.
 */
export const isInDatabase = (url: string): Promise<boolean> => 
  meteorCallAsync("repoCollection.exists")(url)
    .catch((_e: Error) => false)

/**
 * Tries to get repository data from the database.
 * @param url The repository URL.
 * @param notifier Subject to notify about the status.
 * @returns Promise that resolves to the repository data.
 */
export const tryFromDatabaseSerialised = (
  url: string,
  notifier: Subject<string>
): Promise<SerializableRepoData> => new Promise((resolve, reject) => {
  if (notifier != null) notifier.next("Checking database for existing data...");
  meteorCallAsync("repoCollection.getData")(url)
    .then((d: SerializableRepoData) => {
      // TODO CHECK IF REPO DATA IS MOST UP TO DATE
      if (notifier != null) notifier.next("Found data in database!");
      resolve(d);
    })
    .catch((_e: Error) => {
      reject(new Error("Data not found in database"))
    })
})

export const tryFromDatabase = (
  url: string,
  notifier: Subject<string>
): Promise<RepositoryData> => 
  tryFromDatabaseSerialised(url, notifier)
    .then(deserializeRepoData)
    
/**
 * Caches repository data in the database.
 * @param url The repository URL.
 * @param data The repository data to cache.
 */
export const cacheIntoDatabase = (url: string, data: RepositoryData): Promise<void> => 
  meteorCallAsync("repoCollection.insertOrUpdateRepoData")(
    url,
    serializeRepoData(data)
  )

