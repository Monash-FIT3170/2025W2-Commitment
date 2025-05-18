
Hi! Please read below to see a summary of what this API contains:

---------------------------------------------------------
commitment.ts:
a file containing the main functions of the API

function fetchDataFrom(url: string):
returns data parsed from formulateRepoData once it has cloned the repo to the local machine and 
then deletes the data after it has finished cloning

function formulateRepoData(url: string, filepath: string)
a function which returns a RepositoryData object based on a filepath local to the machine. 
You can use this object to capture certain user metrics, such as commits or vice versa.

function createGitRepoStream(url$):
creates an observable object which will emit the repo data once the url$ has emitted a url

---------------------------------------------------------
types.ts:
contains all the types related to storing Git information. 

RepositoryData:
contains all high level information for all repository data
name: string - the name of the repository
branches: BranchData[] - all branch data
allCommits: Map<string, CommitData> - all of the commits within the repository up to when it was cloned

BranchData:
contains all data specific to a branch (what commits are apart of this branch)
branchName: string - self explanitory
commitHashes: string[] - all commit hashes within this branch

CommitData:
contains all information related to a single commit 
commitHash: string - self explanitory
contributor: ContributorData - the contributor who pushed this commit
description: string - the description field inside this commit
timestamp: Date - the timestamp of when this commit was pushed
fileData: FileChanges[] - all file changes of this commit

ContributorData:
contains all information related to a contributor (to be refactored so that multiple names/emails can be mapped to the same user)
name: string - alias of this contributor
email: string - email of this contributor
numCommits: number - the total number of commits this contributor has pushed to the repo

FileChanges:
contains all information related to a file change within a commit
file: FileContents - self explanitory
changes: ChangeData - self explanitory

FileContents:
contains all of the file contents of a file
contents: string - self explanitory
filepath: string - self explanitory

ChangeData:
contains all information about how a file has changed from one commit to the next
char: ChangeType - leading character of the kind of change this is
extra: null | ModifyData | RenameData | CopyData - any additional information needed to see the context of this file within the commit


---------------------------------------------------------
parsers.ts:
contains all parser functions and monads to be used to parse stdout from the git CLI

---------------------------------------------------------
command.ts
a simple function library to help execute commands to the console in the specified directory

---------------------------------------------------------
git_commands.ts
a file dedicated to formulating git-related commands to be used in parsing information from git

---------------------------------------------------------
helpers.ts
a small function library to help transform data in a pure way

