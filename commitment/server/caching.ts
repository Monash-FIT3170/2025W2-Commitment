import { Subject } from 'rxjs';
import { error } from 'console';
import { CommitData, ContributorData, RepositoryData, BranchData } from './commitment_api/types';
import { Mongo } from 'meteor/mongo'
import { fetchDataFrom } from './commitment_api/commitment';
import { serialize } from 'v8';


// -------------Types -------------
interface SerializableRepoData {
    name: string; 
    branches: BranchData[]; 
    allCommits: {key:string; value: CommitData}[]; // Map converted to a list of objects
    contributors: {key:string; value: ContributorData}[]; // Map converted to a list of objects
}


export interface ServerRepoData {
  _id?: string
  url: string
  createdAt: Date
  data: RepositoryData
}

// -------------- Helper Functions ----------------
/**
 * Convert RepositoryData's Maps into plain objects to store in DB.
 */
// function serializeRepoData(data: RepositoryData): SerializableRepoData {
//     return {
//         ...data,
//         allCommits: data.allCommits
//             ? Array.from(data.allCommits, ([key, value]) => ({ key, value }))
//             : [],
//         contributors: data.contributors
//             ? Array.from(data.contributors, ([key, value]) => ({ key, value }))
//             : [],
//     };
// };

/**
 * Convert serialized repo data (plain objects) back into RepositoryData with Maps.
 */
function deserializeRepoData(data: SerializableRepoData): RepositoryData {
    return {
        ...data,
        allCommits: new Map((data.allCommits ).map(entry => [entry.key, entry.value])),
        contributors: new Map((data.contributors ).map(entry => [entry.key, entry.value])),
    };
}

// ----------- Meteor Methods ------------------------------

const RepoCollection = new Mongo.Collection<ServerRepoData>('repoCollection');

Meteor.methods({
    async 'repoCollection.insertOrUpdateRepoData'(url: string, data:RepositoryData){
    console.log("Inserting or updating repo data for URL:", url);

    // convert Maps to plain objects before saving 
    console.log("Data type for allCommits in the insertOrUpdateMethod:", typeof data.allCommits);
    console.log("Data type for contributors in the insertOrUpdateMethod:", typeof data.contributors);
    // const serializableRepoData = serializeRepoData(data);
    const serializableRepoData = (data);

    const updateDoc = {
        $set: {
            data: serializableRepoData, 
            createdAt: new Date(),
        }
    }; 


    const result = await RepoCollection.upsertAsync(
        { url },   // filter to find existing doc
        updateDoc  // update operation
    );

    console.log("Upsert result: ", serializableRepoData); 
    return result;

    },
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
        console.log("Inserting repo data for URL:", url);
        console.log('allCommits type:', typeof data.allCommits);
        console.log('allCommits instanceof Map:', data.allCommits instanceof Map);
        console.log('contributors type:', typeof data.contributors);
        console.log('contributors instanceof Map:', data.contributors instanceof Map);


        const s: ServerRepoData = {
            url,
            createdAt: new Date(),
            data: data,
        }
        const result = await RepoCollection.insertAsync(s);
        console.log("Inserted repo data with ID:", result);
        return result; 
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
            const repoDataDeserialized = deserializeRepoData(repoData.data); 
            // const repoDataDeserialized = repoData.data;


            // //convert plain objects back to Maps: 
            // const repositoryData : RepositoryData = {
            //     ...serverData, 
            //     allCommits: new Map(Object.entries(serverData.allCommits)),
            //     contributors: new Map(Object.entries(serverData.contributors)),
            // }
            console.log("Fetched repo data:", repoDataDeserialized);

            //  // return only the RepositoryData
            //  return repositoryData;
            return repoDataDeserialized;

        }
    });


const cacheIntoDatabase = async (url: string, data: RepositoryData): Promise<boolean> => {
    // cache the data into the database TODO
    console.log("Caching data, allCommits size:", data.allCommits.size);
    console.log("Caching data, contributors size:", data.contributors.size);
    
    return new Promise((resolve, reject) => {
        Meteor.call("repoCollection.insertOrUpdateRepoData", url, data, (err, res) => {
        if (err) {
            console.error("Error inserting repo data:", err);
            reject(err);
        } else {
            resolve(res);
        }
        });
    });
    };

export const isInDatabase = async (url: string): Promise<Boolean> => {
    return Meteor.call("repoCollection.exists", url)
};

const tryFromDatabase = async (url: string, notifier: Subject<string>): Promise<RepositoryData> => {
  // try and get it from database 
  notifier.next('Searching database for your repo...');
  const data = await RepoCollection.findOneAsync({ url });
  console.log('Fetched data:', data);
  if (!data) {
    notifier.next('No data found in DB.');
    return Promise.reject('No data found');
  }
  notifier.next('Found your repo!');
  return data;
  };

//   // return found data
//   notifier.next('Found your repo!');
//   return await RepoCollection.findOneAsync({ url });
// };

export const getRepoData = async (url: string, notifier: Subject<string>): Promise<RepositoryData> => {
    try {
        // to update back to this line of code:
        // const data = await tryFromDatabase(url, notifier);
        // delete these two lines later (need to update the URL being tested):
        const data = await fetchDataFrom(url, notifier);
        console.log("fetched data in getRepoData and checking type of allCommits", typeof data.allCommits);
        await cacheIntoDatabase(url, data);

        console.log("data returned from fetchDataFrom: ", data);
        return data; 
    } catch (e) {
        const data = await fetchDataFrom(url, notifier);
        console.log("Fetched data from external source:", data);
        await cacheIntoDatabase(url, data);
        const savedData = await RepoCollection.findOneAsync({ url });
        console.log("Saved data after caching:", savedData);
        return data;
    }
};

    // tryFromDatabase(url, notifier)
    // .then((data) => {
    //     console.log("output from tryfromDatabase: ", data);
    //     return data;
    // })
    // .catch(async (e) => {
    //     const data = await fetchDataFrom(url, notifier);
    //     console.log("Fetched data from external source:", data);
    //     await cacheIntoDatabase(url, data);
    //     return data;
    // });



