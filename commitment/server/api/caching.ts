import { Subject } from "rxjs";
import { Mongo } from "meteor/mongo";
import { Meteor } from "meteor/meteor";
import { check } from "meteor/check";

import {
  RepositoryData,
  SerializableRepoData,
  BranchData,
  SerialisableMapObject,
  CommitData,
  ContributorData,
} from "@api/types";
import { deserializeRepoData, serializeRepoData } from "@api/serialisation";
import { emitValue } from "@api/meteor_interface";
import { isUpToDate } from "./update";

/**
 * COLLECTION OF REPOSITORY METHODS
 */
type SerializableRepoMetaData = Readonly<{
  name: string;
  branches: BranchData[];
  contributors: SerialisableMapObject<string, ContributorData>[]; // Map converted to a list of objects
}>;

interface ServerRepoMetaData {
  _id?: string;
  createdAt: Date;
  url: string;
  data: SerializableRepoMetaData;
}

interface CommitRepoData {
  _id?: string;
  url: string;
  hash: string;
  commit: CommitData;
}

const getCompleteDataFromDatabase = async (url: string): Promise<SerializableRepoData> => {
  const [repoMetaData, repoCommitData]: [ServerRepoMetaData, CommitRepoData[]] = await Promise.all([
    RepoCollection.findOneAsync({ url }),
    CommitCollection.find({ url }).fetchAsync(),
  ]);

  if (repoMetaData === undefined) throw Error(`Could not find url in database: ${url}`);
  const metaData = repoMetaData.data;

  const allCommits = repoCommitData.map(
    (e) =>
      ({
        key: e.hash,
        value: e.commit,
      } as SerialisableMapObject<string, CommitData>)
  );

  return {
    name: metaData.name,
    branches: metaData.branches,
    contributors: metaData.contributors,
    allCommits: allCommits,
  } as SerializableRepoData;
};

const RepoCollection = new Mongo.Collection<ServerRepoMetaData>("repoCollection");
const CommitCollection = new Mongo.Collection<CommitRepoData>("commitCollection");

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

  /**
   * checks whether the entry into the database is the most up to date
   *
   * @method repoCollection.isUpToDate
   * @param {string} url         The URL of the repo to check.
   * @returns {Promise<boolean>} Whether the url is up to date with GIT or not
   */
  async "repoCollection.isUpToDate"(url: string) {
    check(url, String);
    const d = await tryFromDatabaseSerialised(url, null);
    return await isUpToDate(url, d);
  },
});

/**
 * Checks if a repository exists in the database.
 * @param url The repository URL to check.
 * @returns   True if the repository exists, false otherwise.
 */
export const isInDatabase = async (url: string): Promise<boolean> => {
  const doc = await RepoCollection.findOneAsync({ url: url });
  return null !== doc;
};

/**
 * Caches repository data in the database.
 * @param url The repository URL.
 * @param data The repository data to cache.
 */
export const cacheIntoDatabase = async (url: string, data: RepositoryData): Promise<boolean> => {
  const serialData = serializeRepoData(data);
  const s: ServerRepoMetaData = {
    url,
    createdAt: new Date(),
    data: {
      name: serialData.name,
      branches: serialData.branches,
      contributors: serialData.contributors,
    } as SerializableRepoMetaData,
  };

  const cs = serialData.allCommits.map(
    (e) =>
      ({
        url,
        hash: e.key,
        commit: e.value,
      } as CommitRepoData)
  );

  const res = await RepoCollection.upsertAsync(
    { url }, // filter
    { $set: s } // only set/replace the intended fields
  );

  const cRes = (
    await Promise.all(
      cs.map(async (e: CommitRepoData) => {
        return await CommitCollection.upsertAsync({ url, hash: e.hash }, { $set: e });
      })
    )
  ).reduce((acc, i) => (acc && i.numberAffected! > 0 ? true : false), true);

  // res is an object like { numberAffected, insertedId }
  return res.numberAffected! > 0 && cRes;
};

/**
 * removes a repo inside the database
 * throws an error if does not exist
 * @param url url to check
 * @returns   whether the removal was successful
 */
export const removeRepo = async (url: string): Promise<boolean> => {
  const doc = await RepoCollection.findOneAsync({ url: url });
  // only have to do the one call as the methods should be syncronised
  if (null === doc || undefined == doc) return false;
  const res = await RepoCollection.removeAsync({ url });
  const cRes = await CommitCollection.removeAsync({ url });
  return res > 0 && cRes > 0;
};

// -----------------------  WARNING  -----------------------
// IF I SEE THIS OUTSIDE OF THE TESTCASES
// YOU WILL BE SENT TO THE GULAG
export const voidDatabase = async (): Promise<void> => {
  await RepoCollection.removeAsync({});
  await CommitCollection.removeAsync({});
};

/**
 * Tries to get repository data from the database.
 * @param url      The repository URL.
 * @param notifier Subject to notify about the status.
 * @returns        Promise that resolves to the repository data.
 */
export const tryFromDatabaseSerialised = async (
  url: string,
  notifier: Subject<string> | null
): Promise<SerializableRepoData> => {
  const emit = emitValue(notifier);
  emit("Checking database for existing data...");

  if (!isInDatabase(url)) throw Error("Data not found in database");
  emit("Found data in database!");

  return getCompleteDataFromDatabase(url);
};

export const tryFromDatabaseSerialisedViaLatest = async (
  url: string,
  notifier: Subject<string> | null
): Promise<SerializableRepoData> => {
  const d: SerializableRepoData = await tryFromDatabaseSerialised(url, notifier);
  const upToDate: boolean = await isUpToDate(url, d);

  if (!upToDate) {
    const msg = "Repo is not up to date with the latest changes";
    emitValue(notifier)(msg);
    throw Error(msg);
  }

  return d;
};

/**
 * checks the database without checking whether its up to date
 * @param url a url to search for
 * @param notifier a notifier to notify messages to
 * @returns Promise<RepositoryData> where it found the result, rejected results are search misses
 */
export const tryFromDatabase = (
  url: string,
  notifier: Subject<string> | null
): Promise<RepositoryData> => tryFromDatabaseSerialised(url, notifier).then(deserializeRepoData);

/**
 * checks the database, ensuring the data is the most up to date on git
 * @param url a url to search for
 * @param notifier a notifier to notify messages to
 * @returns Promise<RepositoryData> where it found the result, rejected results are search misses
 */
export const tryFromDatabaseViaLatest = (
  url: string,
  notifier: Subject<string> | null
): Promise<RepositoryData> =>
  tryFromDatabaseSerialisedViaLatest(url, notifier).then(deserializeRepoData);
