import { SerializableRepoData, CommitData, Maybe } from "@api/types";

// HELPER GRANULAR FUNCTIONS

export const compareDates = (d1: Date, d2: Date): boolean => d1.valueOf() > d2.valueOf();

export const takeFromBack = <T>(arr: T[], num: number): T[] => arr.slice(-num);

export const join = (arr: string[]): string => arr.reduce((acc, i) => acc + i, "");

export const zip = <T extends any[][]>(...lists: T): { [K in keyof T]: T[K][number] }[] =>
  Array.from(
    { length: Math.min(...lists.map((list) => list.length)) },
    (_, i) => lists.map((list) => list[i]) as { [K in keyof T]: T[K][number] }
  );

export const safeNumber = (value: unknown): Maybe<number> => {
  const n = Number(value);
  return Number.isNaN(n) || !Number.isFinite(n) ? null : n;
};

export const minValue = <T>(arr: T[], f: (v1: T, v2: T) => boolean): T =>
  arr.reduce((v1, v2) => (f(v1, v2) ? v1 : v2), arr[0]);

export const maxValue = <T>(arr: T[], f: (v1: T, v2: T) => boolean): T =>
  arr.reduce((v1, v2) => (f(v1, v2) ? v2 : v1), arr[0]);

// FUNCTIONS THAT USE SerializableRepoData or any other type from Types.ts

export const sortCommitsByDate = (data: SerializableRepoData): CommitData[] =>
  getAllCommits(data).sort(
    (d1: CommitData, d2: CommitData) => d1.timestamp.valueOf() - d2.timestamp.valueOf()
  );

export const getCommitDates = (data: CommitData[]): Date[] => data.map((d) => d.timestamp);

export const getEarliestCommit = (data: CommitData[]): Maybe<CommitData> =>
  data.length !== 0 ? minValue(data, (d1, d2) => compareDates(d1.timestamp, d2.timestamp)) : null;

export const getLatestCommit = (data: CommitData[]): Maybe<CommitData> =>
  data.length !== 0 ? maxValue(data, (d1, d2) => compareDates(d1.timestamp, d2.timestamp)) : null;

export const getEarliestDate = (data: Date[]): Maybe<Date> =>
  data.length !== 0 ? minValue(data, compareDates) : null;

export const getLatestDate = (data: Date[]): Maybe<Date> =>
  data.length !== 0 ? maxValue(data, compareDates) : null;

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

export const getTotalFilesChanged = (data: CommitData[]): number =>
  data.reduce((sum, p) => sum + p.fileData.length, 0);

export const getTotalLinesOfCode = (data: CommitData[]): number =>
  data.reduce(
    (sum, p) => sum + p.fileData.reduce((fileSum, f) => fileSum + f.newLines - f.deletedLines, 0),
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
): number => getTotalLinesOfCode(getCommitsFrom(repoData, contributorName));

export const getLocPerCommitPerContributor = (
  repoData: SerializableRepoData,
  contributorName: string
): number => {
  const totalLOC = getLOCperContributor(repoData, contributorName);
  const commits = getCommitsFrom(repoData, contributorName);
  const commitCount = commits.length;
  return commitCount > 0 && totalLOC !== null ? totalLOC / commitCount : 0;
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

  return uniqueDays.size > 0 ? commits.length / uniqueDays.size : 0;
};
