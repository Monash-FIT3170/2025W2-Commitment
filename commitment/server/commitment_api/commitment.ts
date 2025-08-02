
import { Subject, observeOn, asyncScheduler, throttleTime } from 'rxjs';
import { scan } from 'rxjs/operators';
import pLimit from 'p-limit'

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
	Command,
	executeCommand, 
	guaranteeExecution,
	doesFilePathExist,
	createFilePath,
	deleteAllFromDirectory
} from "./command"

// imports different types of parsers
import { 
	parseCmd,
	exactText,
	assertSuccess,
	parseRepoExists, 
	parseContributorEmails,
	parseRepoBranches,
	parseCommitHashes,
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
	getContributorEmails,
	getCommitDetails,
	getFileDataFromCommit,
	getOldFileDataFromCommit,
	getRepoName
} from "./git_commands"

const withLimit = (lim: number): (<T>(p: Promise<T>) => Promise<T>) => {
	const l = pLimit(lim)
	return <T>(p: Promise<T>) => l(() => p)
}

/** does not have a notifier which updates any frontend subscribers
 * @deprecated
*/
export const getDataFrom = async (url: string): Promise<RepositoryData> => fetchDataFrom(url, new Subject<string>())

/** gets the repository data from the url
 * @deprecated
*/
export const fetchDataFrom = async (url: string, notifier: Subject<string>): Promise<RepositoryData> => {
		
	// creates path to clone repos in if filepath if it doesnt already exist
	const workingDir = process.cwd()
	if (!doesFilePathExist(workingDir)) createFilePath(workingDir + "/cloned-repos/")	

	// validate that the string is a proper url TODO
	// see if repo exists
	notifier.next("Validating repo exists...")

	const execCmdInRoot = guaranteeExecution("")
	// throws an error if it does not exist
	const repoExistsText = await parseCmd(execCmdInRoot(checkIfRepoExists(url)))(parseRepoExists)	

	// if it exists, clone to a path by duplicating from link
	notifier.next("Formulating parsers...")

	const repoNameFromUrl = last(url.split("/"))!

	const repoRelativePath = "/cloned-repos/" + repoNameFromUrl
	const repoAbsPath = workingDir + repoRelativePath

	// if it already exists delete it as it should not exist (maybe formulateRepoData failed)
	if (doesFilePathExist(repoAbsPath)) await deleteAllFromDirectory(repoAbsPath)

	// clone shit and check if it cloned successfully
	notifier.next("Cloning repo...")

	const cloneCommand = await execCmdInRoot(cloneRepo(url, repoAbsPath))
	assertSuccess(cloneCommand) 

	// get shit
	notifier.next("Getting repository data...")
	const repoData = await formulateRepoData(url, repoAbsPath, notifier)

	// clear shit from local storage (BE CAREFUL WITH THIS AS INJECTION ATTACKS COULD HAPPEN)
	await deleteAllFromDirectory(repoAbsPath)

    // send shit out
	notifier.next("Data processed!")

	return repoData
}


const formulateRepoData = async (url: string, path: string, notifier: Subject<string>): Promise<RepositoryData> => {

	// get a function to point to the creation of a shell in the repo path
	const execCmdInRepo = executeCommand(path)
	const guarantee     = guaranteeExecution(path)
	// creates a function to optimise how many promises are inside the event thread sheduling at once (reduces load on the event thead scheduling algorithm)
	const withMyLimit = withLimit(8)
	
	// get all branches
	notifier.next("Searching for branch names...")
	const branchNames = await parseCmd(guarantee(getBranches()))(parseRepoBranches)
	//.catch(e => Promise.reject(new Error("Failed upon searching for branch names")))

	// get all commits 
	notifier.next("Searching for commit hashes...")
	const allCommitHashesListOfList = await Promise.all(branchNames
		.map( async (branch) => 
			await parseCmd(execCmdInRepo(getAllCommitsFrom(branch)))(parseCommitHashes) 
		)
		.map(withMyLimit)
	).catch(e => Promise.reject(new Error("Failed upon searching for commit hashes")))

	// get all unique commit hashes
	const allCommitHashes = [...new Set(allCommitHashesListOfList.flatMap(i => i))]	

	// get all commit data and file data inside the commit data 
	// get all commit meta data
	const commitsFound = allCommitHashes.length
	const commitLoader = new Subject<number>()
	const $totalCommitsLoaded = commitLoader.pipe( 
		observeOn(asyncScheduler),
		scan((acc, next) => acc + next, 0)
	)
	$totalCommitsLoaded.subscribe(loaded => { notifier.next(`Formulating all commit data (${loaded}/${commitsFound})...`) })
	
	const commitMetaData = await Promise.all(
		allCommitHashes.map(async hash => {
			const ret = await parseCmd(execCmdInRepo(getCommitDetails(hash)))(parseCommitData) // parses all commits
			commitLoader.next(1)
			return ret
		})
		.map(withMyLimit)
		
	).catch(e => Promise.reject(new Error("Failed upon formulating commit data")))

	// get all the file data
	const filesFound = commitMetaData.reduce((acc, data) => acc + data.involvedFiles.length, 0)
	const fileLoader = new Subject<number>()
	const $totalFilesLoaded = fileLoader.pipe( 
		observeOn(asyncScheduler),
		scan((acc, next) => acc + next, 0)
	)
	$totalFilesLoaded.subscribe(loaded => { notifier.next(`Formulating all file data (${loaded}/${filesFound})...`) })
	notifier.next(`Found ${ filesFound } distinct file versions across all commits`)

	const allFileData = await Promise.all(
		commitMetaData.map(async commitData => {

			const getFileContents = (cmd: (h: string, f: string) => Command) => (file: string) => 
				parseCmd(execCmdInRepo(cmd(commitData.commitHash, file)))(exactText)

			const newContents = getFileContents(getFileDataFromCommit)
			const oldContents = getFileContents(getOldFileDataFromCommit)

			return await Promise.all(
				commitData.involvedFiles
					.map(async d => { 
						const ret = await parseFileDataFromCommit(newContents, oldContents)(d) // parses all the file data
						fileLoader.next(1)
						return ret	
					}) 
					.map(withMyLimit) // limits the concurrent parsing to just 8 files being read at the same time to reduce strain on OS scheduling
			)
		})
		.map(withMyLimit) // limits the amount of commits parsed at once to reduce the load on the event thread shedulng algorithm
	).catch(e => Promise.reject(new Error("Failed upon loading file data from repo")))

	// join the file data with the commits to form the final structs
	notifier.next(`Joining found files with commit data...`)
	const allCommitData: CommitData[] = await Promise.all(
		zip(commitMetaData, allFileData).map(async ([commitData, fileData]) => ({
				commitHash: commitData.commitHash,
				commitTitle: commitData.commitTitle,
				contributorName: commitData.contributorName,
				description: commitData.description,
				timestamp: new Date(commitData.dateString),
				fileData: fileData
			} as CommitData)
		)
		.map(withMyLimit)
	)

	// get all contributors and create objects for each contributor
	notifier.next("Formulating all contributors...")
	const uniqueNames = [...new Set(allCommitData.map(d => d.contributorName))]
	const nameToEmails = await Promise.all(
		uniqueNames
		.map(async (name) => parseCmd(guarantee(getContributorEmails(name)))(parseContributorEmails) )
		.map(withMyLimit)
	).catch(e => Promise.reject(new Error("Failed upon searching for contributors")))
	const allContributors: ContributorData[] = zip(uniqueNames, nameToEmails).map(([name, emails]) => ({
		name: name,
		emails: emails
	})) 

	const contributorMap = new Map<string, ContributorData>(allContributors.map(c => [c.name, c]))

	// create a map of all commit hashes to their object
	const commitMap = new Map<string, CommitData>(allCommitData.map(c => [c.commitHash, c]))

	// maps the branch to a list of commit hashes
	notifier.next("Linking branches to their commits...")
	const branchToCommitsMap = new Map<string, string[]>( 
		zip(branchNames, allCommitHashesListOfList)
			.map(([branch, commits]) => [branch, commits]) 
	)

	// compile all branch data and group commits into their relavent branches 
	// sort by timestamp of commit relative to all other commits in that branch 
	notifier.next("Formulating all branch data...")
	const branchData: BranchData[] = await Promise.all(
		branchNames
			.map(async branch => ({
				branchName: branch,
				commitHashes: (branchToCommitsMap.get(branch) as string[])
					.sort((h1, h2) => sortCommitByTimeStamp(
						commitMap.get(h1) as CommitData, 
						commitMap.get(h2) as CommitData
					)
				)
			}))
			.map(withMyLimit)
	).catch(e => Promise.reject(new Error("Failed upon formulating branch data")))

	notifier.next("Fetching repo name...")
	const repoName = await parseCmd(execCmdInRepo(getRepoName()))(parseRepoName)
	.catch(e => Promise.reject(new Error("Failed upon searching for the repo name")))

	return Promise.resolve({
		name: repoName,
		branches: branchData,
		allCommits: commitMap,
		contributors: contributorMap
	})
}

