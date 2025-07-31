import { Command, logData, doNotLogData } from './command';

export const checkIfRepoExists = (url: string): Command => ({
  ...doNotLogData,
  cmd: `git ls-remote ${url}`, // tries to get a repo hash from a url
  onSuccess: (command: string) => `✅ Found Repo ${url}`,
  onFail: (command: string, error: Error) => `❌ Could not find Repo ${url}: ${error.message}`,
});

export const cloneRepo = (url: string, targetDirectory: string): Command => ({
  ...doNotLogData,
  cmd: `git clone ${url} \"${targetDirectory}\"`, // clone url to local path
  onSuccess: (command: string) => `✅ Repo successfully cloned to ${targetDirectory}`,
  onFail: (command: string, error: Error) => `❌ Error cloning repo: ${error.message}`,
});

export const getBranches = (): Command => ({
  ...doNotLogData,
  cmd: 'git branch -a', // gets only local branches
});

export const getAllCommitsFrom = (branch: string): Command => ({
  ...doNotLogData,
  cmd: `git log ${branch} --format=%H`, // gets all commits from this branch with the full 40 character SHA-1 hash for complete unambiguity
});

export const getContributorEmails = (name: string): Command => ({
  ...doNotLogData,
  cmd: `git log --author="${name}" --pretty=format:"%ae"`, // gets all contributor emails inside the repo
});

export const getCommitDetails = (hash: string) => ({
  ...doNotLogData,
  cmd: `git show --pretty=format:"%H%n %an%n %ad%n %s%n %b" --diff-filter=ADMRC --name-status ${hash}`,
});

export const getFileDataFromCommit = (hash: string, path: string): Command => ({
  ...doNotLogData,
  cmd: `git show ${hash}:${path}`,
});

export const getOldFileDataFromCommit = (hash: string, path: string): Command => ({
  ...doNotLogData,
  cmd: `git show ${hash}^:${path}`,
});

export const getRepoName = (): Command => ({
  ...doNotLogData,
  cmd: 'git remote get-url origin',
});
