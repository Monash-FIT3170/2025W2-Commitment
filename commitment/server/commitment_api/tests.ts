import { executeCommand } from "./command"
import { parse, parseCmd, exactText, parseRepoExists, parseRepoBranches } from "./parsers"
import { checkIfRepoExists, cloneRepo } from "./commitment"

export const testIfFoundRepoSuccess = async function () {
    const testRepo = "https://github.com/Densetsu152637/test_repo_for_3170"
    return await executeCommand("")(checkIfRepoExists(testRepo))
}

export const testIfFoundRepoFail = async function () {
    const testRepo = "https://github.com/Densetsu152637/yeet_42069"
    return await executeCommand("")(checkIfRepoExists(testRepo))
}

export const testCloning = async function () {
    const testRepo = "https://github.com/Densetsu152637/test_repo_for_3170"
    const last = <T>(arr: T[]): T | undefined => arr.length > 0 ? arr[arr.length - 1] : undefined
    const workingDir = process.cwd()
    
    const executeCommandInRoot = executeCommand("")
    
    // see if repo exists
    const repoExistsText = await parseCmd(executeCommandInRoot(checkIfRepoExists(testRepo)))
    const repoExists = repoExistsText(parseRepoExists)
    if (!repoExists) return "repo does not exist :/"

    // if it exists, clone to a path by duplicating from link
    const repoNameFromUrl = last(testRepo.split("/"))!

    const repoAbsPath = workingDir + "/cloned-repos/" + repoNameFromUrl

    const res = await parseCmd(executeCommandInRoot(cloneRepo(testRepo, repoAbsPath)))

    return res(exactText)
}


