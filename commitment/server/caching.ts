

import { Subject } from "rxjs";
import { CommitData, RepositoryData } from "./commitment_api/types";
import { fetchDataFrom } from "./commitment_api/commitment";
import { 
    cacheRepositoryData, 
    getRepositoryData, 
    isRepositoryCached,
    connectToDatabase 
} from "./src/services/atlas";

const cacheIntoDatabase = async (url: string, data: RepositoryData) => {
    try {
        // Ensure database connection
        await connectToDatabase();
        
        // Convert Maps to objects for MongoDB storage
        const serializedData = {
            name: data.name,
            branches: data.branches,
            allCommits: Object.fromEntries(data.allCommits),
            contributors: Object.fromEntries(data.contributors)
        };
        
        const success = await cacheRepositoryData(url, serializedData);
        return success;
    } catch (error) {
        console.error("Error caching to database:", error);
        return false;
    }
}

export const isInDatabase = async (url: string) => {
    try {
        await connectToDatabase();
        return await isRepositoryCached(url);
    } catch (error) {
        console.error("Error checking database:", error);
        return false;
    }
}

const tryFromDatabase = async (url: string, notifier: Subject<string>): Promise<RepositoryData> => {
    try {
        // Ensure database connection
        await connectToDatabase();
        
        notifier.next("Searching database for your repo...")

        // Check if data exists in database
        if (!await isInDatabase(url)) {
            const s = "Could not find data in the database"
            notifier.next(s)
            return Promise.reject(s)
        }

        // Get data from database
        const cachedData = await getRepositoryData(url);
        
        if (!cachedData) {
            const s = "Data not found in database"
            notifier.next(s)
            return Promise.reject(s)
        }

        // Convert back to RepositoryData format
        const repositoryData: RepositoryData = {
            name: cachedData.name,
            branches: cachedData.branches,
            allCommits: new Map(Object.entries(cachedData.allCommits)),
            contributors: new Map(Object.entries(cachedData.contributors))
        };

        notifier.next("Found your repo!")
        return Promise.resolve(repositoryData);
    } catch (error) {
        console.error("Error retrieving from database:", error);
        return Promise.reject("Database error occurred");
    }
}


export const getRepoData = async (url: string, notifier: Subject<string>): Promise<RepositoryData> => tryFromDatabase(url, notifier)
    .catch(async e => {
        const data = await fetchDataFrom(url, notifier)
        cacheIntoDatabase(url, data)
        return data
    })
