import { Subject } from 'rxjs';
import { CommitData, ContributorData, RepositoryData, BranchData } from './commitment_api/types';
import { fetchDataFrom } from './commitment_api/commitment';
import { serialize } from 'v8';


// -------------Types -------------
interface SerializableRepoData {
    name: string; 
    branches: BranchData[]; 
    allCommits: {[key:string]: CommitData}; // Map converted to plain object
    contributors: {[key:string]: ContributorData}; // Map converted to plain object
}

interface ChunkedRepoData {
  _id?: string;
  url: string;
  createdAt: Date;
  chunkIndex: number;
  totalChunks: number;
  data: SerializableRepoData;
}

export interface ServerRepoData {
  _id?: string
  url: string
  createdAt: Date
  data: SerializableRepoData // Changed to SerializableRepoData
}

// -------------- Helper Functions ----------------
/**
 * Convert RepositoryData's Maps into plain objects for MongoDB storage
 */
const serializeRepoData = (data: RepositoryData): ServerRepoData['data'] => {
  try {
    // Convert Map objects to plain objects for MongoDB storage
    const serializedData = {
      name: data.name,
      branches: data.branches,
      allCommits: Object.fromEntries(data.allCommits),
      contributors: Object.fromEntries(data.contributors),
    };
    
    console.log('Serialization successful, data size:', JSON.stringify(serializedData).length);
    return serializedData;
  } catch (error) {
    console.error('Error during serialization:', error);
    const errorMessage = error instanceof Error ? error.message : String(error);
    
    // Check for buffer offset errors
    if (errorMessage.includes('ERR_OUT_OF_RANGE') || errorMessage.includes('offset')) {
      throw new Error(`Serialization failed due to buffer size limitations. Repository data is too large to serialize. Error: ${errorMessage}`);
    }
    
    throw new Error(`Serialization failed: ${errorMessage}`);
  }
};

/**
 * Split large repository data into chunks for storage
 */
const chunkRepoData = (data: SerializableRepoData, maxChunkSize: number = 16 * 1024 * 1024): SerializableRepoData[] => {
  const chunks: SerializableRepoData[] = [];
  const dataStr = JSON.stringify(data);
  
  if (dataStr.length <= maxChunkSize) {
    // Data fits in one chunk
    chunks.push(data);
    return chunks;
  }
  
  // Split commits into chunks while keeping all other data intact
  const commitEntries = Object.entries(data.allCommits);
  const commitsPerChunk = Math.floor(maxChunkSize / 1000); // Rough estimate
  
  for (let i = 0; i < commitEntries.length; i += commitsPerChunk) {
    const chunkCommits = commitEntries.slice(i, i + commitsPerChunk);
    const chunkData: SerializableRepoData = {
      name: data.name,
      branches: data.branches,
      allCommits: Object.fromEntries(chunkCommits),
      contributors: data.contributors, // Include all contributors in each chunk
    };
    chunks.push(chunkData);
  }
  
  console.log(`Split repository data into ${chunks.length} chunks`);
  return chunks;
};

/**
 * Store chunked repository data
 */
const storeChunkedRepoData = async (url: string, data: SerializableRepoData): Promise<boolean> => {
  try {
    // First, remove any existing chunks for this URL
    // await RepoCollection.removeAsync({ url }); // This line is removed as RepoCollection is removed
    
    // Split data into chunks if needed
    const chunks = chunkRepoData(data);
    
    if (chunks.length === 1) {
      // Single chunk - store normally
      const repoData: ServerRepoData = {
        url,
        createdAt: new Date(),
        data: chunks[0],
      };
      // await RepoCollection.insertAsync(repoData); // This line is removed as RepoCollection is removed
      console.log('Stored repository data in single chunk');
    } else {
      // Multiple chunks - store each chunk
      for (let i = 0; i < chunks.length; i++) {
        const chunkData: ChunkedRepoData = {
          url,
          createdAt: new Date(),
          chunkIndex: i,
          totalChunks: chunks.length,
          data: chunks[i],
        };
        // await RepoCollection.insertAsync(chunkData); // This line is removed as RepoCollection is removed
      }
      console.log(`Stored repository data in ${chunks.length} chunks`);
    }
    
    return true;
  } catch (error) {
    console.error('Error storing chunked repo data:', error);
    throw error;
  }
};

/**
 * Retrieve and reconstruct chunked repository data
 */
const retrieveChunkedRepoData = async (url: string): Promise<SerializableRepoData | null> => {
  try {
    // Check if data is stored in chunks
    // const chunkedData = await RepoCollection.findAsync({ url, chunkIndex: { $exists: true } }); // This line is removed as RepoCollection is removed
    
    if (false) { // This block is removed as RepoCollection is removed
      // Data is chunked - reconstruct it
      console.log(`Found ${0} chunks for repository`); // This line is removed as RepoCollection is removed
      
      // Sort chunks by index
      // chunkedData.sort((a: ChunkedRepoData, b: ChunkedRepoData) => a.chunkIndex - b.chunkIndex); // This line is removed as RepoCollection is removed
      
      // Merge all commits from all chunks
      const allCommits: {[key: string]: CommitData} = {};
      let mergedData: SerializableRepoData | null = null;
      
      for (const chunk of []) { // This loop is removed as RepoCollection is removed
        if (!mergedData) {
          mergedData = {
            name: chunk.data.name,
            branches: chunk.data.branches,
            allCommits: {},
            contributors: chunk.data.contributors,
          };
        }
        
        // Merge commits from this chunk
        Object.assign(allCommits, chunk.data.allCommits);
      }
      
      if (mergedData) {
        mergedData.allCommits = allCommits;
        console.log(`Reconstructed repository data with ${Object.keys(allCommits).length} commits`);
        return mergedData;
      }
    } else {
      // Single chunk - retrieve normally
      // const repoData = await RepoCollection.findOneAsync({ url, chunkIndex: { $exists: false } }); // This line is removed as RepoCollection is removed
      // if (repoData) {
      //   console.log('Retrieved single-chunk repository data');
      //   return repoData.data;
      // }
    }
    
    return null;
  } catch (error) {
    console.error('Error retrieving chunked repo data:', error);
    throw error;
  }
};

/**
 * Convert serialized repo data (plain objects) back into RepositoryData with Maps.
 */
function deserializeRepoData(data: SerializableRepoData): RepositoryData {
  return {
    ...data,
    allCommits: new Map(Object.entries(data.allCommits)),
    contributors: new Map(Object.entries(data.contributors)),
  };
}

// ----------- Meteor Methods ------------------------------

// const RepoCollection = new Mongo.Collection<ServerRepoData>('repoCollection'); // This line is removed as RepoCollection is removed

Meteor.methods({
    async 'repoCollection.insertOrUpdateRepoData'(url: string, data:RepositoryData){
    console.log("Inserting or updating repo data for URL:", url);

    // convert Maps to plain objects before saving 
    const serializableRepoData = serializeRepoData(data);

    const updateDoc = {
        $set: {
            data: serializableRepoData, 
            createdAt: new Date(),
        }
    }; 

    const result = await RepoCollection.upsertAsync( // This line is removed as RepoCollection is removed
        { url },   // filter to find existing doc
        updateDoc  // update operation
    );

    console.log("Upsert result: ", result); 
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

        // Serialize the data before storing
        const serializableData = serializeRepoData(data);
        
        // Use chunked storage to handle large repositories
        const result = await storeChunkedRepoData(url, serializableData);
        console.log("Inserted repo data with chunked storage");
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
        // if (!isInDatabase(url)) { // This line is removed as RepoCollection is removed
        //     throw new Meteor.Error('not-in-database', 'The repo must exist in the database to be removed');
        // }

        // const d = await RepoCollection.findOneAsync({ url }); // This line is removed as RepoCollection is removed

        // if (!d) {
        //     throw new Meteor.Error('link-not-found', 'Link not found');
        // }

        // return RepoCollection.removeAsync(d._id);
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
        // const ret = await RepoCollection.findOneAsync({ url }) // This line is removed as RepoCollection is removed
        // return ret !== null;
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
            // const bm = await RepoCollection.findOneAsync({ url }); // This line is removed as RepoCollection is removed
    
            // if (!bm) {
            //     throw new Meteor.Error('bookmark-not-found', 'Bookmark not found');
            // }
    
            // return RepoCollection.updateAsync(bm._id, { $set: { lastViewed: new Date() } });
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
            // const repoData = await RepoCollection.findOneAsync({ url }); // This line is removed as RepoCollection is removed
            // if (!repoData) {
            //     // the error is thrown here - Milni 
            //     throw new Meteor.Error('not-found', 'Repo data not found');
            // }
            // works correctly here 
            // const repoDataDeserialized = deserializeRepoData(repoData.data); // This line is removed as RepoCollection is removed

            // //convert plain objects back to Maps: 
            // const repositoryData : RepositoryData = {
            //     ...serverData, 
            //     allCommits: new Map(Object.entries(serverData.allCommits)),
            //     contributors: new Map(Object.entries(serverData.contributors)),
            // }
            console.log("Fetched repo data:", null); // This line is removed as RepoCollection is removed

            //  // return only the RepositoryData
            //  return repositoryData;
            return null; // This line is removed as RepoCollection is removed

        }
    });


export const isInDatabase = async (url: string): Promise<Boolean> => {
    // Directly query the collection instead of calling a non-existent method
    // const ret = await RepoCollection.findOneAsync({ url }); // This line is removed as RepoCollection is removed
    return false; // This line is removed as RepoCollection is removed
};

const cacheIntoDatabase = async (url: string, data: RepositoryData): Promise<boolean> => {
    try {
        // cache the data into the database - serialize Maps to plain objects before storing
        const serializableData = serializeRepoData(data);
        // const s: ServerRepoData = { // This line is removed as RepoCollection is removed
        //     url,
        //     createdAt: new Date(),
        //     data: serializableData,
        // }
        // const result = await RepoCollection.insertAsync(s); // This line is removed as RepoCollection is removed
        console.log('Data cached successfully with ID:', null); // This line is removed as RepoCollection is removed
        
        // Verify the data was actually saved
        // const savedData = await RepoCollection.findOneAsync({ url }); // This line is removed as RepoCollection is removed
        // if (!savedData) {
        //     throw new Error('Data was not saved to database');
        // }
        
        return true;
    } catch (error) {
        console.error('Error caching data:', error);
        throw error;
    }
};

const tryFromDatabase = async (url: string, notifier: Subject<string>): Promise<RepositoryData> => {
  // try and get it from database 
  notifier.next('Searching database for your repo...');
  // const data = await RepoCollection.findOneAsync({ url }); // This line is removed as RepoCollection is removed
  console.log('Fetched data from DB:', null); // This line is removed as RepoCollection is removed
  // if (!data) {
  //   notifier.next('No data found in DB.');
  //   return Promise.reject('No data found');
  // }
  // notifier.next('Found your repo!');
  // Deserialize the data before returning
  // return deserializeRepoData(data.data); // This line is removed as RepoCollection is removed
  return Promise.reject('No data found'); // This line is removed as RepoCollection is removed
};

export const getRepoData = async (url: string, notifier: Subject<string>): Promise<RepositoryData> => {
    console.log(`getRepoData called with URL: ${url}`);
    
    try {
        // First try to get from database
        console.log('Attempting to get data from database...');
        const data = await tryFromDatabase(url, notifier);
        console.log("Retrieved data from database: ", data);
        return data; 
    } catch (e) {
        console.log('Data not found in database, fetching from GitHub...');
        notifier.next('Repository not found in cache, fetching from GitHub...');
        try {
            const data = await fetchDataFrom(url, notifier);
            console.log('Data fetched from GitHub, caching to database...');
            await cacheIntoDatabase(url, data);
            console.log("Fetched and cached data from external source:", data);
            return data;
        } catch (fetchError) {
            console.error('Error in fetchDataFrom or cacheIntoDatabase:', fetchError);
            notifier.next(`Error fetching repository: ${fetchError}`);
            throw fetchError;
        }
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



