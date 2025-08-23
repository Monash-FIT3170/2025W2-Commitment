import { Subject } from "rxjs";
import { Mongo } from "meteor/mongo";

import { RepositoryData, BranchData, CommitData, ContributorData } from "../imports/api/types";

// -------------Types -------------

type SerialisableMapObject<K, V> = {
  key: K;
  value: V;
};

export type SerializableRepoData = Readonly<{
  name: string;
  branches: BranchData[];
  allCommits: SerialisableMapObject<string, CommitData>[]; // Map converted to a list of objects
  contributors: SerialisableMapObject<string, ContributorData>[]; // Map converted to a list of objects
}>;

export interface ServerRepoData {
  _id?: string;
  url: string;
  createdAt: Date;
  data: SerializableRepoData;
}

// -------------- Helper Functions ----------------
/**
 * Convert RepositoryData's Maps into plain objects to store in DB.
 */

const mapToArray = <K, V>(m: unknown): SerialisableMapObject<K, V>[] => {
  if (m instanceof Map) {
    return Array.from(m.entries()).map(([key, value]) => ({ key, value }))
  } else if (m && typeof m === "object") {
    // plain object fallback
    return Object.entries(m).map(([key, value]) => ({
      key: key as K,
      value: value as V,
    }))
  }
  return []
};

const arrayToMap = <K, V>(a: unknown): Map<K, V> => {
  if (a instanceof Array){
    return new Map(a.map(e => [e.key as K, e.value as V]))
  } else if (a && typeof a == "object") {
    // object fallback
    return new Map(Object.entries(a).map(e => [e[0] as K, e[1] as V]))
  }
  return new Map()
}

function serializeRepoData(data: RepositoryData): SerializableRepoData {
  return {
    ...data,
    allCommits: mapToArray<string, CommitData>(data.allCommits),
    contributors: mapToArray<string, ContributorData>(data.contributors),
  }
}

function deserializeRepoData(data: SerializableRepoData): RepositoryData {
  return {
    ...data,
    allCommits: arrayToMap<string, CommitData>(data.allCommits),
    contributors: arrayToMap<string, ContributorData>(data.contributors),
  }
}

/**
 * COLLECTION OF REPOSITORY METHODS
 */
const RepoCollection = new Mongo.Collection<ServerRepoData>("repoCollection");

Meteor.methods({
  /**
   * Inserts a new link into the LinksCollection.
   *
   * @method links.insert
   * @param {string} url - The URL of the link. Must start with 'http' or 'https'.
   * @param {string} data - the repo metadata to be saved
   * @returns {Promise<string>} The ID of the newly inserted link document.
   * @throws {Meteor.Error} If the URL is invalid or does not start with 'http', not in db or not authorised.
   */
  async "repoCollection.insertOrUpdateRepoData"(url: string, data: RepositoryData) {
    const s: ServerRepoData = {
      url,
      createdAt: new Date(),
      data: serializeRepoData(data),
    };
    return await RepoCollection.upsertAsync(
      { url }, // filter to find existing doc
      s // update operation
    );
  },

  /**
   * Removes a repo from the RepoCollection by its URL.
   *
   * @method links.remove
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
   * @method links.isBookmarked
   * @param {string} url - The URL to check.
   * @returns {Promise<boolean>} True if the URL is bookmarked, false otherwise.
   * @throws {Meteor.Error} If no link with the given URL is found or not authorised.
   */
  async "repoCollection.exists"(url: string) {
    const ret = await RepoCollection.findOneAsync({ url });
    return ret !== null;
  },

  /**
   * Updates the lastViewed parameter of the bookmark.
   *
   * @method bookmarks.updateLastViewed
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
   *
   * @param {string} url - The URL of the repository.
   *
   * @returns {Promise<RepositoryData>} The repository data.
   * @throws {Meteor.Error} If the repository data is not found or not authorised.
   *
   */
  async "repoCollection.getData"(url: string) {
    const repoData = await RepoCollection.findOneAsync({ url });

    if (!repoData) {
      throw new Meteor.Error("not-found", "Repo data not found");
    }

    return deserializeRepoData(repoData.data);
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
  new Promise((resolve, reject) => {
    Meteor.call(
      "repoCollection.insertOrUpdateRepoData",
      url,
      data,
      (err: any, res: boolean | PromiseLike<boolean>) => {
        if (err) reject(err);
        else resolve(res);
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
      (err: Error, res: RepositoryData | PromiseLike<RepositoryData>) => {
        if (err) {
          const s = "Couldn't find your repo in the database";
          notifier.next(s);
          return reject(s);
        }

        notifier.next("Found your repo in the database!");
        return resolve(res);
      }
    );
  });
