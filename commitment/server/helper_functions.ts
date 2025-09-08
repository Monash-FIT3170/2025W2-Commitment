import {
  SerializableRepoData,
  RepositoryData,
  BranchData,
  CommitData,
  FileChanges,
  ChangeType,
  ContributorData
} from "../imports/api/types"

// HELPER GRANULAR FUNCTIONS

export const zip = <T extends any[][]>(...lists: T): { [K in keyof T]: T[K][number] }[] => 
  Array.from({ length: Math.min(...lists.map(
    (list) => list.length)) }, (_, i) => 
      lists.map((list) => list[i]) as { [K in keyof T]: T[K][number] }
    );

export const getLinesOfCodeFromCommit = (commit: CommitData): number => 
  commit.fileData.reduce((acc, f) => acc + f.newLines - f.deletedLines, 0);

// FUNCTIONS THAT USE SerializableRepoData

export const getBranchNames = (data: SerializableRepoData): string[] => 
  data.branches.map((b) => b.branchName)

export const getTotalBranches = (data: SerializableRepoData): number => 
  data.branches.length

export const getContributors = (data: SerializableRepoData): string[] =>
  data.contributors.map((p) => p.value.name)

export const getNumberOfContributors = (data: SerializableRepoData): number => 
    getContributors(data).length

export const getRepoName = (data: SerializableRepoData): string =>
  data.name

export const getTotalCommits = (data: SerializableRepoData): number =>
  data.allCommits.length

export const getTotalFilesChanged = (repoData: SerializableRepoData): number =>
  repoData.allCommits.reduce((sum, p) => 
    sum + p.value.fileData.length
  , 0)

export const getTotalLinesOfCode = (repoData: SerializableRepoData): number =>
  repoData.allCommits.reduce((sum, p) => 
    sum + p.value.fileData.reduce(
      (fileSum, f) => fileSum + f.newLines - f.deletedLines
      , 0
    ), 0)

export const getAllContributorsCommitCounts = (
  data: SerializableRepoData
): { name: string; value: number }[] => {
  // acceptible use of mutables as it is way more efficient then the alternative
  const counts: Record<string, number> = {};
  
  data.allCommits.forEach(commit => {
    const contributor = commit.value.contributorName;
    counts[contributor] = (counts[contributor] || 0) + 1;
  });
  
  return Object.entries(counts).map(([name, value]) => ({ name, value }));
}

export const getTotalLocDataSerializable = (
  data: SerializableRepoData
): { name: string; value: number }[] => 
  getContributors(data)
    .map(c => ({ 
      name: c, 
      value: getTotalCommitsPerContributor(data, c) 
    }))

// FUNCTIONS THAT USE SerializableRepoData + contributorName (for targeted metrics)

export const getCommitsFrom = (
  data: SerializableRepoData, 
  name: string
): CommitData[] => 
  data.allCommits
    .filter(p => p.value.contributorName == name)
    .map(p => p.value)

export const getTotalCommitsPerContributor = (
  repoData: SerializableRepoData,
  contributorName: string
): number =>
  getCommitsFrom(repoData, contributorName).length

export const getLOCperContributor = (
  repoData: SerializableRepoData,
  contributorName: string
): number => 
  getCommitsFrom(repoData, contributorName)
    .reduce((acc, commit) => 
      acc + commit.fileData.reduce((acc, fileChange) =>
        acc + fileChange.newLines - fileChange.deletedLines
      , 0)
    , 0
    ) 

export const getLocPerCommitPerContributor = (
  repoData: SerializableRepoData,
  contributorName: string
): number => {
  const totalLOC = getLOCperContributor(repoData, contributorName)
  const commits = getCommitsFrom(repoData, contributorName)
  return commits.length === 0 ? 0 : totalLOC / commits.length
}

export const getCommitPerDayPerContributor = (
  repoData: SerializableRepoData,
  contributorName: string
): number => {
  // get commits filtered according to contributors
  const commits = getCommitsFrom(repoData, contributorName)
  // count unique days
  const uniqueDays = new Set(
    // extract commit dates as (YYYY-MM-DD)
    commits.map((commit) => 
      commit.timestamp.toISOString().split("T")[0]
    )
  )  
  return commits.length / uniqueDays.size
}