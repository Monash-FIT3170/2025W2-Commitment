import { Subject } from "rxjs";

import { fetchDataFrom, getDataFrom } from "./commitment"

export const testFullFetchFunctionality = async (testRepo: string, subject: Subject<string>) => {

    const res = fetchDataFrom(testRepo, subject)

    return res.then(d => JSON.stringify({
        name: d.name,
        branches: d.branches,
        allCommits: [...d.allCommits],
        contributors: [...d.contributors]
    }, null, 2))
}

export const testFullGetFunctionality = async (testRepo: string) => {

    const res = getDataFrom(testRepo)
    
    return res.then(d => JSON.stringify({
        name: d.name,
        branches: d.branches,
        allCommits: [...d.allCommits],
        contributors: [...d.contributors]
    }, null, 2))
}

export const testGetRepoGiven1 = async () => {
    const testRepo = "https://github.com/Densetsu152637/test_repo_for_3170" 
    return testFullGetFunctionality(testRepo)
}

export const testFetchRepoGiven1 = async () => {
    const testRepo = "https://github.com/Densetsu152637/test_repo_for_3170" 
    const s = new Subject<string>()
    s.subscribe(console.log)
    return testFullFetchFunctionality(testRepo, s)
}