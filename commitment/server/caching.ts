
import { Subject } from "rxjs";
import { CommitData, RepositoryData } from "../commitment_api/types";
import { fetchDataFrom } from "../commitment_api/commitment";



const cacheIntoDatabase = async (d: RepositoryData) => {
    // cache the data into the database TODO


}


const tryFromDatabase = async (url: string, notifier: Subject<string>): Promise<RepositoryData> => {
    // try and get it from database TODO
    notifier.next("Searching database for your repo...")

    // if data not found, reject promise TODO
    const s = "Could not find data in the database"
    notifier.next(s)
    return Promise.reject(s)

    // return found data
    notifier.next("Found your repo!")
}


export const getRepoData = async (url: string, notifier: Subject<string>): Promise<RepositoryData> => tryFromDatabase(url, notifier)
    .catch(async e => {
        const data = await fetchDataFrom(url, notifier)
        cacheIntoDatabase(data)
        return data
    })
