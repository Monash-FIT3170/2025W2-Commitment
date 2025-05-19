
import { Observable, Subject, map } from "rxjs";

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
	parseCmdImmediate,
	exactText,
	parseSuccess,
	parseRepoExists, 
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

// does not have a notifier which updates any frontend subscribers
export const getDataFrom = async (url: string): Promise<RepositoryData> => fetchDataFrom(url, new Subject<string>())

// gets the repository data from the url
export const fetchDataFrom = async (url: string, notifier: Subject<string>): Promise<RepositoryData> => {
		
	// creates path to clone repos in if filepath if it doesnt already exist
	const workingDir = process.cwd()
	if (!doesFilepathExist(workingDir)) createFilePath(workingDir + "/cloned-repos/")	

	// validate that the string is a proper url TODO
	// see if repo exists
	notifier.next("Validating repo exists...")

	const execCmdInRoot = executeCommand("")
	// throws an error if it does not exist
	const repoExistsText = await parseCmdImmediate(execCmdInRoot(checkIfRepoExists(url)))(parseRepoExists)	

	// if it exists, clone to a path by duplicating from link
	notifier.next("Formulating parsers...")

	const repoNameFromUrl = last(url.split("/"))!

	const repoRelativePath = "/cloned-repos/" + repoNameFromUrl
	const repoAbsPath = workingDir + repoRelativePath

	// if it already exists delete it as it should not exist (maybe formulateRepoData failed)
	if (doesFilepathExist(repoAbsPath)) await deleteAllFromDirectory(repoAbsPath)

	// clone shit and check if it cloned successfully
	notifier.next("Cloning repo...")

	const cloneCommand = execCmdInRoot(cloneRepo(url, repoAbsPath))
	const _ignoredCloneResult = await parseSuccess(cloneCommand) 

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
	
	// get all branches
	notifier.next("Searching for branch names...")
	const branchNames = await parseCmdImmediate(execCmdInRepo(getBranches()))(parseRepoBranches)

	// get all commits (runs co-currently :D)
	notifier.next("Searching for commit hashes...")
	const allCommitHashesListOfList = await Promise.all(branchNames
		.map( async (branch) => await parseCmdImmediate(execCmdInRepo(getAllCommitsFrom(branch)))(parseCommitHashes) )
	)

	// get all unique commit hashes
	const allCommitHashes = [...new Set(allCommitHashesListOfList.flatMap(i => i))]	

	// get all commit data and file data inside the commit data (multithreaded)
	notifier.next("Formulating all commit data...")
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
				contributorName: commitData.contributorName,
				description: commitData.description,
				timestamp: new Date(commitData.dateString),
				fileData: allFileData
			} as CommitData)
		})
	)

	// get all contributors and create objects for each contributor
	notifier.next("Formulating all contributors...")
	const uniqueNames = [...new Set(allCommitData.map(d => d.contributorName))]
	const nameToEmails = await Promise.all(
		uniqueNames.map(async (name) => await parseCmdImmediate(execCmdInRepo(getContributorEmails(name)))(t => t.split("\n")))
	)
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
		zip(branchNames, allCommitHashesListOfList).map(([branch, commits]) => [branch, commits]) 
	)

	// compile all branch data and group commits into their relavent branches 
	// sort by timestamp of commit relative to all other commits in that branch (multithreaded)
	notifier.next("Formulating all branch data...")
	const branchData: BranchData[] = await Promise.all(branchNames.map(async branch => ({
			branchName: branch,
			commitHashes: (branchToCommitsMap.get(branch) as string[])
				.sort((h1, h2) => {
					const [c1, c2] = [commitMap.get(h1) as CommitData, commitMap.get(h2) as CommitData]
					const t1 = c1!.timestamp.getTime()
					const t2 = c2!.timestamp.getTime()
					return t1 - t2 
			})
		}))
	)	

	const repoName = await parseCmdImmediate(execCmdInRepo(getRepoName()))(parseRepoName)

	return Promise.resolve({
		name: repoName,
		branches: branchData,
		allCommits: commitMap,
		contributors: contributorMap
	})
}

