import { Subject } from "rxjs";
import { Mongo } from "meteor/mongo";
import { Meteor } from "meteor/meteor";
import { check } from "meteor/check";
import { RepositoryData, SerializableRepoData } from "/imports/api/types";
import { deserializeRepoData, serializeRepoData } from "/imports/api/serialisation";
import { meteorCallAsync, override, overrideValue } from "/imports/api/meteor_interface";
import { isUpToDate } from "./update";

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
   * Removes a repo from the RepoCollection by its URL.
   *
   * @method repoCollection.removeRepo
   * @param {string} url - The URL of the link to be removed.
   * @returns {Promise<boolean>} whether the removal was successful
   * @throws {Meteor.Error} If no link with the given URL is found or not authorised.
   */
  async "repoCollection.removeRepo"(url: string): Promise<boolean> {
    check(url, String);
    return removeRepo(url);
  },

  /**
   * Checks whether a link with the given URL exists in the repoCollection.
   *
   * @method repoCollection.exists
   * @param {string} url         The URL to check.
   * @returns {Promise<boolean>} True if the URL is bookmarked, false otherwise.
   * @throws {Meteor.Error}      If no link with the given URL is found or not authorised.
   */
  async "repoCollection.exists"(url: string): Promise<boolean> {
    check(url, String);
    return isInDatabase(url);
  },

  /**
   * Updates the lastViewed parameter of the repository.
   *
   * @method repoCollection.updateLastViewed
   * @param {string} url        The URL of the repo to update.
   * @returns {Promise<number>} The number of documents updated (should be 1 if successful).
   * @throws {Meteor.Error}     If the URL is invalid, repo not found, or not authorised.
   */
  async "repoCollection.updateLastViewed"(url: string) {
    check(url, String);
    const bm = await RepoCollection.findOneAsync({ url });

    if (!bm) {
      throw new Meteor.Error("bookmark-not-found", "Bookmark not found");
    }

    return await RepoCollection.updateAsync({ url }, { $set: { lastViewed: new Date() } });
  },
});

/**
 * Checks if a repository exists in the database.
 * @param url The repository URL to check.
 * @returns   True if the repository exists, false otherwise.
 */
export const isInDatabase = async (url: string): Promise<boolean> => {
  const doc = await RepoCollection.findOneAsync({ url });
  return null !== doc;
};

/**
 * Caches repository data in the database.
 * @param url The repository URL.
 * @param data The repository data to cache.
 */
export const cacheIntoDatabase = async (url: string, data: RepositoryData): Promise<boolean> => {
  const s: ServerRepoData = {
    url,
    createdAt: new Date(),
    data: serializeRepoData(data),
  };

  return await RepoCollection.upsertAsync(
    { url }, // filter to find existing doc
    s // update operation
  )
    .then((_d: any) => true)
    .catch(overrideValue(false));
};

/**
 * removes a repo inside the database
 * throws an error if does not exist
 * @param url url to check
 * @returns   whether the removal was successful
 */
export const removeRepo = async (url: string): Promise<boolean> => {
  const existing = await RepoCollection.findOneAsync({ url });
  if (!existing) throw Error(`url ${url} is not in the database`);
  const res = await RepoCollection.removeAsync({ url });
  return res > 0;
};

// should be used carefully as this exposes all data in the database
export const allUrls = (): Promise<string[]> =>
  RepoCollection.find()
    .fetch()
    .then((d: ServerRepoData[]) => d.map((d: ServerRepoData) => d.url));

// -----------------------  WARNING  -----------------------
// IF I SEE THIS OUTSIDE OF THE TESTCASES
// YOU WILL BE SENT TO THE GULAG
export const voidDatabase = async (): Promise<boolean[]> => {
  const urls: string[] = await allUrls();
  return await Promise.all(urls.map(removeRepo));
};

/**
 * Tries to get repository data from the database.
 * @param url      The repository URL.
 * @param notifier Subject to notify about the status.
 * @returns        Promise that resolves to the repository data.
 */
export const tryFromDatabaseSerialised = async (
  url: string,
  notifier: Subject<string>
): Promise<SerializableRepoData> => {
  if (notifier != null) notifier.next("Checking database for existing data...");

  const repoData = await RepoCollection.findOneAsync({ url });
  if (!repoData) throw Error("Data not found in database");
  if (notifier != null) notifier.next("Found data in database!");

  return repoData.data;
};

export const tryFromDatabaseSerialisedViaLatest = async (
  url: string,
  notifier: Subject<string>
): Promise<SerializableRepoData> => {
  const d: SerializableRepoData = await tryFromDatabaseSerialised(url, notifier);
  const upToDate: boolean = await isUpToDate(url, d);

  if (!upToDate) throw Error("Repo is not up to date with the latest changes");
  return d;
};

/**
 * checks the database, ensuring the data is the most up to date on git
 * @param url a url to search for
 * @param notifier a notifier to notify messages to
 * @returns Promise<RepositoryData> where it found the result, rejected results are search misses
 */
export const tryFromDatabase = (url: string, notifier: Subject<string>): Promise<RepositoryData> =>
  tryFromDatabaseSerialisedViaLatest(url, notifier).then(deserializeRepoData);

/**
 * checks the database without checking whether its up to date
 * @param url a url to search for
 * @param notifier a notifier to notify messages to
 * @returns Promise<RepositoryData> where it found the result, rejected results are search misses
 */
export const tryFromDatabaseNoCheck = (
  url: string,
  notifier: Subject<string>
): Promise<RepositoryData> => tryFromDatabaseSerialised(url, notifier).then(deserializeRepoData);
