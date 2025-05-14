
import { Observable, map } from "rxjs";

// imports a list of helper functions
import { last, zip } from "./helpers"

// imports types
import { 
	RepositoryData,
	BranchData,
	CommitData,
	ContributorData,
	sortCommitByTimeStamp
} from "./types";

// imports all command functions
import { 
	executeCommand, 
	doesFilepathExist,
	createFilePath,
	deleteAllFromDirectory
} from "./command"

// imports different types of parsers
import { 
	Maybe,
	parseCmdImmediate,
	exactText,
	parseSuccess,
	parseRepoExists, 
	parseRepoBranches,
	parseCommitHashes,
	parseContributorData,
	parseCommitData,
	parseRepoName,
	parseFileDataFromCommit
} from "./parsers"

// imports different commands that will run on the repo
import {
	checkIfRepoExists,
	cloneRepo,
	getBranches,
	getAllCommitsFrom,
	getAllContributors,
	getCommitDetails,
	getFileDataFromCommit,
	getOldFileDataFromCommit,
	getRepoName
} from "./git_commands"

// NOTE: need to implement a cache outside of this function i think (when the database is complete)
export const fetchDataFrom = async (url: string): Promise<Maybe<RepositoryData>> => promiseDataFrom(url).catch((error) => {
	// log error TODO (implement a logging system)
	
	// return a null value instead of getting an error
	return null
})


const promiseDataFrom = async (url: string): Promise<RepositoryData> => {
	
	// creates path to clone repos in if filepath if it doesnt already exist
	const workingDir = process.cwd()
	if (!doesFilepathExist(workingDir)) createFilePath(workingDir + "/cloned-repos/")	

	// validate that the string is a proper url TODO
	// see if repo exists
	const execCmdInRoot = executeCommand("")
	// throws an error if it does not exist
	const repoExistsText = await parseCmdImmediate(execCmdInRoot(checkIfRepoExists(url)))(parseRepoExists)	

	// if it exists, clone to a path by duplicating from link
	const repoNameFromUrl = last(url.split("/"))!

	const repoRelativePath = "/cloned-repos/" + repoNameFromUrl
	const repoAbsPath = workingDir + repoRelativePath

	// if it already exists delete it as it should not exist
	if (doesFilepathExist(repoAbsPath)) {
		await deleteAllFromDirectory(repoAbsPath)
	} 

	// clone shit and check if it cloned successfully
	const cloneCommand = execCmdInRoot(cloneRepo(url, repoAbsPath))
	const _ignoredCloneResult = await parseSuccess(cloneCommand) 

	// get shit
	const repoData = await formulateRepoData(url, repoAbsPath)

	// clear shit from local storage (BE CAREFUL WITH THIS AS INJECTION ATTACKS COULD HAPPEN)
	await deleteAllFromDirectory(repoAbsPath)

    // send shit out
	return Promise.resolve(repoData)
}


const formulateRepoData = async (url: string, path: string): Promise<RepositoryData> => {

	// get a function to point to the creation of a shell in the repo path
	const execCmdInRepo = executeCommand(path)
	
	// get all branches
	const branchNames = await parseCmdImmediate(execCmdInRepo(getBranches()))(parseRepoBranches)

	// get all commits (runs co-currently :D)
	const allCommitHashesListOfList = await Promise.all(branchNames
		.map( async (branch) => await parseCmdImmediate(execCmdInRepo(getAllCommitsFrom(branch)))(parseCommitHashes) )
	)
	// get all unique commit hashes
	const allCommitHashes = [... new Set(allCommitHashesListOfList.flatMap(i => i))]

	// get all contributors and create objects for each contributor
	const allContributors: ContributorData[] = await parseCmdImmediate(execCmdInRepo(getAllContributors()))(parseContributorData)

	// create a map of all contributor names to their object
	const contributorMap = new Map<string, ContributorData>(allContributors.map(c => [c.name, c]))

	// get all commit data and file data inside the commit data (multithreaded)
	const allCommitData: CommitData[] = await Promise.all(
		allCommitHashes.map(async hash => { 
			const commitData = await parseCmdImmediate(execCmdInRepo(getCommitDetails(hash)))(parseCommitData)

			const getFileContents    = (file: string) => parseCmdImmediate(execCmdInRepo(getFileDataFromCommit(commitData.commitHash, file)))(exactText)
			const getOldFileContents = (file: string) => parseCmdImmediate(execCmdInRepo(getOldFileDataFromCommit(commitData.commitHash, file)))(exactText)

			const allFileData = await Promise.all(
				commitData.involvedFiles.map(parseFileDataFromCommit(getFileContents, getOldFileContents))
			)

			return ({
				commitHash: commitData.commitHash,
				contributor: contributorMap.get(commitData.contributorName),
				description: commitData.description,
				timestamp: new Date(commitData.dateString),
				fileData: allFileData
			} as CommitData)
		})
	)

	// create a map of all commit hashes to their object
	const commitMap = new Map<string, CommitData>(allCommitData.map(c => [c.commitHash, c]))

	// maps the branch to a list of commit hashes
	const branchToCommitsMap = new Map<string, string[]>( 
		zip(branchNames, allCommitHashesListOfList).map(([branch, commits]) => [branch, commits]) 
	)

	// compile all branch data and group commits into their relavent branches 
	// sort by timestamp of commit relative to all other commits in that branch (multithreaded)
	const branchData: BranchData[] = await Promise.all(branchNames.map(async branch => ({
			branchName: branch,
			commitHashes: (branchToCommitsMap.get(branch) as string[])
				.sort((h1, h2) => commitMap.get(h1)!.timestamp.getTime() - commitMap.get(h2)!.timestamp.getTime())
		}))
	)

	const repoName = await parseCmdImmediate(execCmdInRepo(getRepoName(url)))(parseRepoName)

	return {
		name: repoName,
		branches: branchData,
		allCommits: commitMap
	}
}

export const createGitRepoStream = <T>(url$: Observable<T>, f: (t: T) => Promise<RepositoryData | null>): Observable<RepositoryData | null> => {
	const o$ = new Observable<RepositoryData | null>()
	url$.pipe(
		map((t: T) => {
			o$.pipe(
				map(async () => { await f(t) })
			)
		})
	);
	return o$
}

