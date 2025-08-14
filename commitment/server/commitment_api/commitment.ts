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

function serializeRepoData(data: RepositoryData): SerializableRepoData {
    return {
        ...data,
        allCommits: data.allCommits
            ? Array.from(data.allCommits, ([key, value]) => ({ key, value }))
            : [],
        contributors: data.contributors
            ? Array.from(data.contributors, ([key, value]) => ({ key, value }))
            : [],
    };
};

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
  const repoExistsText = await parseCmdImmediate(execCmdInRoot(checkIfRepoExists(url)))(parseRepoExists);

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

  const serializedData = serializeRepoData(repoData);
  console.log("Checking type of AllCommits in serialized data", typeof serializedData.allCommits);
  
  return serializedData;
};

const formulateRepoData = async (url: string, path: string, notifier: Subject<string>): Promise<RepositoryData> => {
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

  // get all commit data and file data inside the commit data (multithreaded)
  notifier.next('Formulating all commit data...');
  const allCommitData: CommitData[] = await Promise.all(
    allCommitHashes.map(async (hash) => {
      const commitData = await parseCmdImmediate(execCmdInRepo(getCommitDetails(hash)))(parseCommitData);

      const getFileContents = (file: string) => parseCmdImmediate(execCmdInRepo(getFileDataFromCommit(commitData.commitHash, file)))(exactText);
      const getOldFileContents = (file: string) => parseCmdImmediate(execCmdInRepo(getOldFileDataFromCommit(commitData.commitHash, file)))(exactText);

      const allFileData = await Promise.all(
        commitData.involvedFiles.map(parseFileDataFromCommit(getFileContents, getOldFileContents)),
      );

      return ({
        commitHash: commitData.commitHash,
        commitTitle: commitData.commitTitle,
        contributorName: commitData.contributorName,
        description: commitData.description,
        timestamp: new Date(commitData.dateString),
        fileData: allFileData,
      } as CommitData);
    }),
  ).catch((e) => Promise.reject(new Error('Failed upon formulating commit data')));

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
};
