import { Observable, Subject, map } from 'rxjs';

// imports a list of helper functions
import { last, zip } from './helpers';

// imports types
import {
  RepositoryData,
  BranchData,
  CommitData,
  ContributorData,
  sortCommitByTimeStamp,
} from './types';

// imports all command functions
import {
  executeCommand,
  doesFilePathExist,
  createFilePath,
  deleteAllFromDirectory,
} from './command';

// imports different types of parsers
import {
  parseCmdImmediate,
  exactText,
  parseSuccess,
  parseRepoExists,
  parseContributorEmails,
  parseRepoBranches,
  parseCommitHashes,
  parseCommitData,
  parseRepoName,
  parseFileDataFromCommit,
} from './parsers';

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
  getRepoName,
} from './git_commands';

// does not have a notifier which updates any frontend subscribers
export const getDataFrom = async (url: string): Promise<RepositoryData> => fetchDataFrom(url, new Subject<string>());

// gets the repository data from the url
export const fetchDataFrom = async (url: string, notifier: Subject<string>): Promise<RepositoryData> => {
  // creates path to clone repos in if filepath if it doesnt already exist
  const workingDir = process.cwd();
  if (!doesFilePathExist(workingDir)) createFilePath(`${workingDir}/cloned-repos/`);

  // validate that the string is a proper url TODO
  // see if repo exists
  notifier.next('Validating repo exists...');

  const execCmdInRoot = executeCommand('');
  // throws an error if it does not exist
  try {
    const repoExistsText = await parseCmdImmediate(execCmdInRoot(checkIfRepoExists(url)))(parseRepoExists);
    if (!repoExistsText) {
      throw new Error(`Repository does not exist or is not accessible: ${url}`);
    }
  } catch (error) {
    const errorMessage = error instanceof Error ? error.message : String(error);
    notifier.next(`Repository validation failed: ${errorMessage}`);
    throw new Error(`Repository validation failed: ${errorMessage}`);
  }

  // if it exists, clone to a path by duplicating from link
  notifier.next('Formulating parsers...');

  const repoNameFromUrl = last(url.split('/'))!;

  const repoRelativePath = `/cloned-repos/${repoNameFromUrl}`;
  const repoAbsPath = workingDir + repoRelativePath;

  // if it already exists delete it as it should not exist (maybe formulateRepoData failed)
  if (doesFilePathExist(repoAbsPath)) await deleteAllFromDirectory(repoAbsPath);

  // clone shit and check if it cloned successfully
  notifier.next('Cloning repo...');

  const cloneCommand = execCmdInRoot(cloneRepo(url, repoAbsPath));
  const _ignoredCloneResult = await parseSuccess(cloneCommand);

  // get shit
  notifier.next('Getting repository data...');
  const repoData = await formulateRepoData(url, repoAbsPath, notifier);

  // clear shit from local storage (BE CAREFUL WITH THIS AS INJECTION ATTACKS COULD HAPPEN)
  await deleteAllFromDirectory(repoAbsPath);

  // send shit out
  notifier.next('Data processed!');
  console.log("Sending repo data from commitment.ts:", repoData);
  
  try {
    return repoData;
  } catch (error) {
    console.error('Error returning repo data:', error);
    const errorMessage = error instanceof Error ? error.message : String(error);
    
    // Check for buffer offset errors
    if (errorMessage.includes('ERR_OUT_OF_RANGE') || errorMessage.includes('offset')) {
      throw new Error(`Repository data is too large to process. This repository contains too many commits or files. Error: ${errorMessage}`);
    }
    
    throw error;
  }
};

const formulateRepoData = async (url: string, path: string, notifier: Subject<string>): Promise<RepositoryData> => {
  try {
    // get a function to point to the creation of a shell in the repo path
    const execCmdInRepo = executeCommand(path);

    // get all branches
    notifier.next('Searching for branch names...');
    const branchNames = await parseCmdImmediate(execCmdInRepo(getBranches()))(parseRepoBranches).catch((e) => Promise.reject(new Error('Failed upon searching for branch names')));

    // get all commits (runs co-currently :D)
    notifier.next('Searching for commit hashes...');
    const allCommitHashesListOfList = await Promise.all(branchNames
      .map(async (branch) => await parseCmdImmediate(execCmdInRepo(getAllCommitsFrom(branch)))(parseCommitHashes))).catch((e) => Promise.reject(new Error('Failed upon searching for commit hashes')));

    // get all unique commit hashes
    const allCommitHashes = [...new Set(allCommitHashesListOfList.flatMap((i) => i))];
    
    // Limit commits for very large repositories to prevent buffer overflow
    const MAX_COMMITS = 1000; // Limit to 1000 commits to prevent serialization issues
    const limitedCommitHashes = allCommitHashes.length > MAX_COMMITS 
      ? allCommitHashes.slice(0, MAX_COMMITS) 
      : allCommitHashes;
    
    if (allCommitHashes.length > MAX_COMMITS) {
      notifier.next(`Repository is very large (${allCommitHashes.length} commits). Processing first ${MAX_COMMITS} commits to prevent errors.`);
      console.warn(`Repository has ${allCommitHashes.length} commits, limiting to ${MAX_COMMITS} to prevent buffer overflow`);
    }

    // get all commit data and file data inside the commit data (multithreaded)
    notifier.next('Formulating all commit data...');
    const allCommitData: CommitData[] = await Promise.all(
      limitedCommitHashes.map(async (hash) => {
        try {
          const commitData = await parseCmdImmediate(execCmdInRepo(getCommitDetails(hash)))(parseCommitData);
          
          if (!commitData) {
            throw new Error(`Failed to parse commit data for hash: ${hash}`);
          }

          const getFileContents = (file: string) => parseCmdImmediate(execCmdInRepo(getFileDataFromCommit(commitData.commitHash, file)))(exactText);
          const getOldFileContents = (file: string) => parseCmdImmediate(execCmdInRepo(getOldFileDataFromCommit(commitData.commitHash, file)))(exactText);

          const allFileData = await Promise.all(
            commitData.involvedFiles.map(async (fileData) => {
              try {
                const result = await parseFileDataFromCommit(getFileContents, getOldFileContents)(fileData);
                return result;
              } catch (error) {
                console.warn(`Error processing file data for commit ${commitData.commitHash}:`, error);
                // Return null for files that can't be processed, but don't crash the entire commit
                return null;
              }
            })
          );

          // Filter out null results (files that couldn't be processed)
          const validFileData = allFileData.filter(data => data !== null);

          return ({
            commitHash: commitData.commitHash,
            commitTitle: commitData.commitTitle,
            contributorName: commitData.contributorName,
            description: commitData.description,
            timestamp: new Date(commitData.dateString),
            fileData: validFileData,
          } as CommitData);
        } catch (error) {
          console.error(`Error processing commit ${hash}:`, error);
          const errorMessage = error instanceof Error ? error.message : String(error);
          
          // Check for specific buffer offset errors
          if (errorMessage.includes('ERR_OUT_OF_RANGE') || errorMessage.includes('offset')) {
            console.warn(`Buffer offset error for commit ${hash}, skipping this commit`);
            // Return a minimal commit data structure to avoid crashing
            return {
              commitHash: hash,
              commitTitle: 'Error processing commit',
              contributorName: 'Unknown',
              description: 'Commit data could not be processed due to buffer error',
              timestamp: new Date(),
              fileData: [],
            } as CommitData;
          }
          
          throw new Error(`Failed to process commit ${hash}: ${errorMessage}`);
        }
      }),
    ).catch((e) => Promise.reject(new Error(`Failed upon formulating commit data: ${e.message}`)));

    // get all contributors and create objects for each contributor
    notifier.next('Formulating all contributors...');
    const uniqueNames = [...new Set(allCommitData.map((d) => d.contributorName))];
    const nameToEmails = await Promise.all(
      uniqueNames.map(async (name) => await parseCmdImmediate(execCmdInRepo(getContributorEmails(name)))(parseContributorEmails)),
    ).catch((e) => Promise.reject(new Error('Failed upon searching for contributors')));
    const allContributors: ContributorData[] = zip(uniqueNames, nameToEmails).map(([name, emails]) => ({
      name,
      emails,
    }));

    const contributorMap = new Map<string, ContributorData>(allContributors.map((c) => [c.name, c]));

    // create a map of all commit hashes to their object
    const commitMap = new Map<string, CommitData>(allCommitData.map((c) => [c.commitHash, c]));

    // maps the branch to a list of commit hashes
    notifier.next('Linking branches to their commits...');
    const branchToCommitsMap = new Map<string, string[]>(
      zip(branchNames, allCommitHashesListOfList).map(([branch, commits]) => [branch, commits]),
    );

    // compile all branch data and group commits into their relavent branches
    // sort by timestamp of commit relative to all other commits in that branch (multithreaded)
    notifier.next('Formulating all branch data...');
    const branchData: BranchData[] = await Promise.all(branchNames.map(async (branch) => ({
      branchName: branch,
      commitHashes: (branchToCommitsMap.get(branch) as string[])
        .sort((h1, h2) => sortCommitByTimeStamp(
						commitMap.get(h1) as CommitData,
						commitMap.get(h2) as CommitData,
        )),
    }))).catch((e) => Promise.reject(new Error('Failed upon formulating branch data')));

    notifier.next('Fetching repo name...');
    const repoName = await parseCmdImmediate(execCmdInRepo(getRepoName()))(parseRepoName)
      .catch((e) => Promise.reject(new Error('Failed upon searching for the repo name')));

    return Promise.resolve({
      name: repoName,
      branches: branchData,
      allCommits: commitMap,
      contributors: contributorMap,
    });
  } catch (error) {
    console.error('Error in formulateRepoData:', error);
    const errorMessage = error instanceof Error ? error.message : String(error);
    
    // Check for buffer offset errors
    if (errorMessage.includes('ERR_OUT_OF_RANGE') || errorMessage.includes('offset')) {
      throw new Error(`Repository processing failed due to buffer size limitations. This repository may be too large to process completely. Error: ${errorMessage}`);
    }
    
    throw error; // Re-throw other errors
  }
};
