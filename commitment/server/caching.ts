
import { Subject } from "rxjs";
import { CommitData, RepositoryData } from "./commitment_api/types";
import { fetchDataFrom } from "./commitment_api/commitment";
import { error } from "console";

const cacheIntoDatabase = async (url: string, data: RepositoryData) => {
    // cache the data into the database TODO


}

export const isInDatabase = async (url: string) => {
    return false
}

const tryFromDatabase = async (url: string, notifier: Subject<string>): Promise<RepositoryData> => {
    // try and get it from database TODO
    notifier.next("Searching database for your repo...")

    // if data not found, reject promise TODO
    if (!await isInDatabase(url)) {
        const s = "Could not find data in the database"
        notifier.next(s)
        return Promise.reject(s)
    }

    // return found data
    notifier.next("Found your repo!")
    return Promise.resolve({
        name: "",
        branches: [],
        allCommits: new Map(),
        contributors: new Map()
    })
}


export const getRepoData = async (url: string, notifier: Subject<string>): Promise<RepositoryData> => 
    tryFromDatabase(url, notifier)
    .catch(async e => {
        const data = await fetchDataFromHaskellApp(url, notifier)
        cacheIntoDatabase(url, data)
        return data
    })


const fetchDataFromHaskellApp = async (url: string, notifier: Subject<string>): Promise<RepositoryData> => new Promise<RepositoryData>((resolve, reject) => {
        const socket = new WebSocket("ws://localhost:8081")

        socket.onopen = () => {
            // Step 1: Send repo URL
            socket.send(JSON.stringify({
                url: url
            }))
        }

        socket.onmessage = (event) => {
            // Step 2: Await response from haskell app
            const data = event.data;
            const parsed = JSON.parse(data);

            if (parsed.type === "text_update") notifier.next(data.replace("text_update: ", ""))
            else if (parsed.type === "value") resolve(data.value) 
            else if (parsed.type === "error") reject(parsed.message)
        }

        socket.onclose = () => {}
    })
