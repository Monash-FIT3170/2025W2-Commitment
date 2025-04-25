
import { Command, logData, doNotLogData } from "./command"


export const checkIfRepoExists = (url: string): Command => ({
    ...doNotLogData,
    cmd: `git ls-remote ${url}`, // tries to get a repo hash from a url
    onSuccess: (command: string)             => `✅ Found Repo ${url}`,
    onFail:    (command: string, error: Error)  => `❌ Could not find Repo ${url}: ${error.message}`
}) 

export const cloneRepo = (url: string, targetDirectory: string): Command => ({
    ...doNotLogData,
    cmd: `git clone ${url} \"${targetDirectory}\"`, // clone url to local path
    onSuccess: (command: string)                => `✅ Repo successfully cloned to ${targetDirectory}`,
    onFail:    (command: string, error: Error)  => `❌ Error cloning repo: ${error.message}`
})

export const getBranches = (): Command => ({
    ...doNotLogData,
    cmd: `git branch -a`, // gets all branches, local and remote
})

export const getAllCommitsFrom = (branch: string): Command => ({
    ...doNotLogData,
    cmd: `git log --oneline ${branch}`, // gets all branches, local and remote
})

export const getAllContributors = (): Command => ({
    ...doNotLogData,
    cmd: `git shortlog -s -n -e | awk '{ count=$1; $1=""; sub(/^ /, ""); print count "|" $0 }'`, // gets all contributor data from the repo and delimits the information with |
})

export const getCommitDetails = (hash: string) => ({
    ...doNotLogData,
    cmd: 
`git show --pretty=format:"%H%n%an%n%ad%n%s" --diff-filter=ADMRC --no-patch --name-status ${hash} |
awk '{
  if ($1 ~ /^R[0-9]+$/ || $1 ~ /^C[0-9]+$/) {
    # rename or copy, has 3 columns: status from to
    print $1 "\t" $2 "|||" $3
  } else {
    print $0
  }
}'`
})

export const getFileDataFromCommit = (hash: string, path: string): Command => ({
    ...doNotLogData,
    cmd: `git show ${hash}:${path}`
})

export const getOldFileDataFromCommit = (hash: string, path: string): Command => ({
    ...doNotLogData,
    cmd: `git show ${hash}^:${path}`
})

export const getRepoName = (url: string): Command => ({
    ...doNotLogData,
    cmd: `basename -s .git ${url}` 
})