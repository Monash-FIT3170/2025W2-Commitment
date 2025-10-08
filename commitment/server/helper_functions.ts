import { SerializableRepoData, CommitData } from "@api/types";

// HELPER GRANULAR FUNCTIONS

export const compareDates = (d1: Date, d2: Date): boolean => d1.valueOf() > d2.valueOf();

export const takeFromBack = <T>(arr: T[], num: number): T[] => arr.slice(-num);

export const join = (arr: string[]): string => arr.reduce((acc, i) => acc + i, "");

export const zip = <T extends any[][]>(...lists: T): { [K in keyof T]: T[K][number] }[] =>
  Array.from(
    { length: Math.min(...lists.map((list) => list.length)) },
    (_, i) => lists.map((list) => list[i]) as { [K in keyof T]: T[K][number] }
  );

export const safeNumber = (value: unknown): number => {
  const n = Number(value);
  return Number.isNaN(n) || !Number.isFinite(n) ? 0 : n;
};

// FUNCTIONS THAT USE SerializableRepoData or any other type from Types.ts

export const sortCommitsByDate = (data: SerializableRepoData): CommitData[] =>
  getAllCommits(data).sort(
    (d1: CommitData, d2: CommitData) => d1.timestamp.valueOf() - d2.timestamp.valueOf()
  );

export const getEarliestCommit = (data: SerializableRepoData): CommitData | null =>
  getAllCommits(data).reduce(
    (acc: CommitData | null, c: CommitData) =>
      acc && compareDates(acc.timestamp, new Date(c.timestamp)) ? c : acc,
    null
  );

export const getLatestCommit = (data: SerializableRepoData): CommitData | null =>
  getAllCommits(data).reduce(
    (acc: CommitData | null, c: CommitData) =>
      acc && compareDates(acc.timestamp, new Date(c.timestamp)) ? acc : c,
    null
  );

export const getLinesOfCodeFromCommit = (commit: CommitData): number =>
  commit.fileData.reduce((acc, f) => acc + f.newLines - f.deletedLines, 0);

export const getBranchNames = (data: SerializableRepoData): string[] =>
  data.branches.map((b) => b.branchName);

export const getAllCommits = (data: SerializableRepoData): CommitData[] =>
  data.allCommits.map((p) => p.value);

export const getTotalBranches = (data: SerializableRepoData): number => data.branches.length;

export const getContributors = (data: SerializableRepoData): string[] =>
  data.contributors.map((p) => p.value.name);

export const getNumberOfContributors = (data: SerializableRepoData): number =>
  getContributors(data).length;

export const getRepoName = (data: SerializableRepoData): string => data.name;

export const getTotalCommits = (data: SerializableRepoData): number => data.allCommits.length;

export const getTotalFilesChanged = (repoData: SerializableRepoData): number =>
  repoData.allCommits.reduce((sum, p) => sum + p.value.fileData.length, 0);

export const getTotalLinesOfCode = (repoData: SerializableRepoData): number =>
  repoData.allCommits.reduce(
    (sum, p) =>
      sum + p.value.fileData.reduce((fileSum, f) => fileSum + f.newLines - f.deletedLines, 0),
    0
  );

export const getAllContributorsCommitCounts = (
  data: SerializableRepoData
): { name: string; value: number }[] => {
  // acceptible use of mutables as it is way more efficient then the alternative
  const counts: Record<string, number> = {};

  data.allCommits.forEach((commit) => {
    const contributor = commit.value.contributorName;
    counts[contributor] = (counts[contributor] || 0) + 1;
  });

  return Object.entries(counts).map(([name, value]) => ({ name, value }));
};

export const getTotalLocDataSerializable = (
  data: SerializableRepoData
): { name: string; value: number }[] =>
  getContributors(data).map((c) => ({
    name: c,
    value: getTotalCommitsPerContributor(data, c),
  }));

// FUNCTIONS THAT USE SerializableRepoData + contributorName (for targeted metrics)

export const getCommitsFrom = (data: SerializableRepoData, name: string): CommitData[] =>
  data.allCommits.filter((p) => p.value.contributorName === name).map((p) => p.value);

export const getTotalCommitsPerContributor = (
  repoData: SerializableRepoData,
  contributorName: string
): number => {
  const commits = getCommitsFrom(repoData, contributorName);
  const count = Number(commits.length ?? 0);
  return Number.isNaN(count) ? 0 : count;
};

export const getLOCperContributor = (
  repoData: SerializableRepoData,
  contributorName: string
): number =>
  getCommitsFrom(repoData, contributorName).reduce((acc, commit) => {
    const fileLOC = (commit.fileData ?? []).reduce((innerAcc, fileChange) => {
      const added = safeNumber(fileChange?.newLines);
      const deleted = safeNumber(fileChange?.deletedLines);
      return innerAcc + (added - deleted);
    }, 0);
    return acc + fileLOC;
  }, 0);

export const getLocPerCommitPerContributor = (
  repoData: SerializableRepoData,
  contributorName: string
): number => {
  const totalLOC = getLOCperContributor(repoData, contributorName);
  const commits = getCommitsFrom(repoData, contributorName);
  const commitCount = safeNumber(commits.length);
  return commitCount === 0 ? 0 : safeNumber(totalLOC / commitCount);
};

export const getCommitPerDayPerContributor = (
  repoData: SerializableRepoData,
  contributorName: string
): number => {
  const commits = getCommitsFrom(repoData, contributorName);

  const uniqueDays = new Set(
    commits.map((commit) => {
      const date = new Date(commit.timestamp);
      return date.toISOString().split("T")[0];
    })
  );

  return safeNumber(commits.length / uniqueDays.size);
};
