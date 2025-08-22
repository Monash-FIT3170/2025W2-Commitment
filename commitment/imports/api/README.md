Hi! Please read below to see a summary of API type documentation:

---

RepositoryData:
contains all high level information for all repository data
name: string - the name of the repository
branches: BranchData[] - all branch data
allCommits: Map<string, CommitData> - all of the commits within the repository up to when it was cloned
allContributors: Map<string, ContributorData> - all of the contributors to this repository

BranchData:
contains all data specific to a branch (what commits are apart of this branch)
branchName: string - self explanitory
commitHashes: string[] - all commit hashes within this branch

CommitData:
contains all information related to a single commit
commitHash: string - self explanitory
contributor: string - the name of the contributor who pushed this commit
description: string - the description field inside this commit
timestamp: Date - the timestamp of when this commit was pushed
fileData: FileChanges[] - all file changes of this commit

ContributorData:
contains all information related to a contributor (to be refactored so that multiple names/emails can be mapped to the same user)
name: string - alias of this contributor
emails: string - all emails linked to this name

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
