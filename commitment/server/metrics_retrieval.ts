import { Subject } from "rxjs"
import { getRepoData } from "./caching";
import { RepositoryData } from "./commitment_api/types";


// const testFunction = (data: RepositoryData): void => {}

// const metricsFunctions = new Map<string, <T>(data: RepositoryData) => T>([
//     ["something", testFunction]
// ])

// Meteor.methods({
//     async "getMetricFromRepo" <T>(repoUrl: string, metricFunctionName: string) { 

//         const repo = await getRepoData(repoUrl, new Subject<string>())
//         const res = metricsFunctions.get(metricFunctionName)

//         if (res == null) {
//             return null
//         }

//         return res(repo)
//     } 
// })