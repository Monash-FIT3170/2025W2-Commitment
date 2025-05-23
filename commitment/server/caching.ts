
import { Subject } from "rxjs";
import { CommitData, RepositoryData } from "../commitment_api/types";
import { fetchDataFrom } from "../commitment_api/commitment";

const cacheIntoDatabase = async (url: string, data: RepositoryData) => {
    // cache the data into the database TODO


}

export const isInDatabase = async (url: string) => {
    return false
}

const tryFromDatabase = async (url: string, notifier: Subject<string>): Promise<RepositoryData> => {
    // try and get it from database TODO
    notifier.next("Searching database for your repo...")
    const found = await isInDatabase(url)

    // if data not found, reject promise TODO
    if (!found) {
        const s = "Could not find data in the database"
        notifier.next(s)
        return Promise.reject(s)
    }

    // return found data
    notifier.next("Found your repo!")
    return Promise.resolve(null)
}


export const getRepoData = async (url: string, notifier: Subject<string>): Promise<RepositoryData> => tryFromDatabase(url, notifier)
    .catch(async e => {
        const data = await fetchDataFrom(url, notifier)
        cacheIntoDatabase(url, data)
        return data
    })
