import { Subject } from 'rxjs';
import { error } from 'console';
import { CommitData, RepositoryData } from './commitment_api/types';
import { Mongo } from 'meteor/mongo'
import { fetchDataFrom } from './commitment_api/commitment';

export interface ServerRepoData {
  _id?: string
  url: string
  createdAt: Date
  data: RepositoryData
}

const RepoCollection = new Mongo.Collection<ServerRepoData>('repoCollection');

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
    async 'repoCollection.insertRepoData'(url: string, data: RepositoryData) {
        const s: ServerRepoData = {
            url,
            createdAt: new Date(),
            data: data,
        }
        return await RepoCollection.insertAsync(s);
    },

    /**
     * Removes a repo from the RepoCollection by its URL.
     *
     * @method links.remove
     * @param {string} url - The URL of the link to be removed.
     * @returns {Promise<number>} The number of documents removed (should be 1 if successful).
     * @throws {Meteor.Error} If no link with the given URL is found or not authorised.
     */
    async 'repoCollection.removeRepo'(url: string) {
        if (!isInDatabase(url)) {
            throw new Meteor.Error('not-in-database', 'The repo must exist in the database to be removed');
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
     * @param {string} url - The URL to check.
     * @returns {Promise<boolean>} True if the URL is bookmarked, false otherwise.
     * @throws {Meteor.Error} If no link with the given URL is found or not authorised.
     */
    async 'repoCollection.exists'(url: string) {
        const ret = await RepoCollection.findOneAsync({ url })
        return ret !== null;
    },

        /**
     * Updates the lastViewed parameter of the bookmark.
     *
     * @method bookmarks.updateLastViewed
     * @param {string} url - The URL of the repository to update.
     * @returns {Promise<number>} The number of documents updated (should be 1 if successful).
     * @throws {Meteor.Error} If the URL is invalid, bookmark not found, or not authorised.
     */
         async 'repoCollection.updateLastViewed'(url: string) {
            const bm = await RepoCollection.findOneAsync({ url });
    
            if (!bm) {
                throw new Meteor.Error('bookmark-not-found', 'Bookmark not found');
            }
    
            return RepoCollection.updateAsync(bm._id, { $set: { lastViewed: new Date() } });
        }, 
        /**
         * Get repository data by URL - added method by Milni in order to actually retrieve saved repo data 
         * @method repoCollection.getRepoData
         * @param {string} url - The URL of the repository.
         * @returns {Promise<RepositoryData>} The repository data.
         * @throws {Meteor.Error} If the repository data is not found or not authorised.
         * 
         */
        async 'repoCollection.getData'(url: string) {
            // this has been working correctly 
            console.log(`Fetching repo data for URL: ${url}`);
            const repoData = await RepoCollection.findOneAsync({ url });
            if (!repoData) {
                // the error is thrown here - Milni 
                throw new Meteor.Error('not-found', 'Repo data not found');
            }
            // works correctly here 
            console.log("Fetched repo data:", repoData);
            return repoData.data;  // return only the RepositoryData 
        },
    });

const cacheIntoDatabase = async (url: string, data: RepositoryData): Promise<boolean> => {
    // cache the data into the database TODO
    return Meteor.call("repoCollection.insertRepoData", url, data)
};

export const isInDatabase = async (url: string): Promise<Boolean> => {
    return Meteor.call("repoCollection.exists", url)
};

const tryFromDatabase = async (url: string, notifier: Subject<string>): Promise<RepositoryData> => {
  // try and get it from database 
  notifier.next('Searching database for your repo...');

  // if data not found, reject promise 
  if (!await isInDatabase(url)) {
    console.log(`No data found in database for URL: ${url}`);
    const s = 'Could not find data in the database';
    notifier.next(s);
    return Promise.reject(s);
  }

  // return found data
  notifier.next('Found your repo!');
  return Promise.resolve(RepoCollection.findOneAsync({ url }));
};

export const getRepoData = async (url: string, notifier: Subject<string>): Promise<RepositoryData> => 
    tryFromDatabase(url, notifier)
    .catch(async (e) => {
        const data = await fetchDataFrom(url, notifier);
        console.log("Fetched data from external source:", data);
        await cacheIntoDatabase(url, data);
        return data;
    });



