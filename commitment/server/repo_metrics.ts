import {
  MetricsData,
  SerializableRepoData,
  FilteredData,
  HighlightStruct,
  LeaderboardData,
  LineGraphData,
  PieChartData,
} from "../imports/api/types";
import { Meteor } from "meteor/meteor";

// storing a global access unfiltered data here
let unfilteredRepoData = {} as Promise<SerializableRepoData>;

// -------- THIS FUNCTION NEEDS TO BE CALLED FIRST -----------------------
export async function getAllMetrics(data: FilteredData): Promise<MetricsData> {
  // set the unfiltered data we will use for all other metrics
  setsUnfilteredData(data.repoUrl);

  // get all the metrics based on the AnalyticsData structure
  return {
    highlights: {
      totalCommits: await highlightTotalCommits(data),
      totalLinesOfCode: await highlightTotalLinesOfCode(data),
      numContributors: numContributors(data),
      numBranches: numBranches(data),
    },
    contributors: {
      leaderboard: leaderboardData(data),
      lineGraph: lineGraphData(data),
      pieChart: pieChartData(data),
    },
  };
}

/**
 * SETTERS AND GETTERS
 */

/**
 * Fetch unfiltered repository data from the database.
 * @param repoUrl The URL of the repository.
 * @returns A promise that resolves to the unfiltered repository data.
 */
export function setsUnfilteredData(repoUrl: string) {
  // implementation of fetched repo data from the database
  // set global variable:
  unfilteredRepoData = Meteor.callAsync("repoCollection.getData", repoUrl);
}

export function getUnfilteredData(): Promise<SerializableRepoData> {
  return unfilteredRepoData;
}

/** Branch names
 * @param data Repository Data
 * @returns Array of branch names
 */
export function getBranches(data: FilteredData): string[] {
  return data.repositoryData.branches.map((b) => b.branchName);
}

/** Contributors (from RepositoryData.contributors map)
 * @param data Repository Data
 * @returns Array of contributor names
 */
export function getContributors(data: FilteredData): string[] {
  return data.repositoryData.contributors.map((c) => c.value.name);
}

/**
 * Get the repository name.
 * @param data Repository Data
 * @returns String of the repo name
 */
export function getRepoName(data: FilteredData): string {
  return data.repositoryData.name;
}

// --------------------------- ALL RELEVANT METRICS --------------------------
/**
 * Get the percentage change of commits in a repository.
 * @param startDate The start date for the comparison.
 * @param data The filtered repository data.
 * @returns The percentage change of commits.
 */
async function percentageCommitChange(
  startDate: Date,
  data: SerializableRepoData
): Promise<number> {
  // get original data
  const unfilteredData = await getUnfilteredData();

  // get the length of commits from the beginning up until the start date
  const prevCommits = unfilteredData.allCommits.filter(
    (c) => new Date(c.value.timestamp).getTime() < startDate.getTime()
  ).length;
  // get the length of commits in the filtered data
  const currentCommits = data.allCommits.length;

  // calculate the percentage change
  const pChange = ((currentCommits - prevCommits) / prevCommits) * 100;

  return pChange;
}

export function getTotalCommits(data: SerializableRepoData): number {
  return data.allCommits.length;
}

/**
 * Returns the total commits in a repository for a highlight card.
 * @param data Filtered Repository Data
 * @returns Highlighted total commits information
 */
export async function highlightTotalCommits(
  data: FilteredData
): Promise<HighlightStruct> {
  const repoData = data.repositoryData;
  const totalCommits = getTotalCommits(repoData);

  // calculate percentage change
  const pChange = await percentageCommitChange(
    data.dateRange.start,
    repoData
  );

  const commitsData = repoData.allCommits.map(({ value }) => ({ value: 1 })); // each commit counts as 1

  return {
    total: totalCommits,
    percentageChange: pChange,
    isPositive: pChange > 0,
    data: commitsData,
  };
}

/**
 * Get the total number of files changed in a repository.
 * @param repoData The repository data.
 * @returns The total number of files changed.
 */
export function getTotalFilesChanged(repoData: SerializableRepoData): number {
  return repoData.allCommits.reduce<number>((sum, commit) => {
    const numFiles = commit.value.fileData.length;
    return numFiles + sum;
  }, 0);
}

/**
 * Returns the total lines of code(total files changed) in the repository for the highlight card.
 * @param data Filtered Repository Data
 * @returns Highlighted total lines of code information
 */
export async function highlightTotalLinesOfCode(
  data: FilteredData
): Promise<HighlightStruct> {
  const repoData = data.repositoryData;

  // number of files changed
  const currentFiles = getTotalFilesChanged(repoData);
  const unfilteredData = await getUnfilteredData();

  const prevFiles = getTotalFilesChanged({
    ...unfilteredData,
    allCommits: unfilteredData.allCommits.filter(
      (c) =>
        new Date(c.value.timestamp).getTime() < data.dateRange.start.getTime()
    ),
  });

  // calculate percentage change
  const percentageChange =
    prevFiles === 0 ? 100 : ((currentFiles - prevFiles) / prevFiles) * 100;

  return {
    total: currentFiles,
    percentageChange,
    isPositive: percentageChange > 0,
    data: repoData.allCommits.map((c) => ({ value: c.value.fileData.length })),
  };
}

/**
 * Returns the number of contributors in the repository for the highlight card.
 * @param data Filtered Repository Data
 * @returns Highlighted number of contributors information
 */
export function numContributors(data: FilteredData): number {
  return data.repositoryData.contributors.length;
}

/**
 * Returns the number of branches in the repository for the highlight card.
 * @param data Filtered Repository Data
 * @returns Highlighted number of branches information
 */
export function numBranches(data: FilteredData): number {
  return data.repositoryData.branches.length;
}

/**
 * Returns the leaderboard data for contributors in the repository.
 * @param data Filtered Repository Data
 * @returns Leaderboard data for contributors
 */
export function leaderboardData(data: FilteredData): LeaderboardData[] {
  const counts: Record<string, number> = {};
  const repoData = data.repositoryData;

  // count commits per contributor
  repoData.allCommits.forEach((commit) => {
    const user = commit.value.contributorName;
    counts[user] = (counts[user] ?? 0) + 1;
  });

  const leaderboard: LeaderboardData[] = Object.entries(counts).map(
    ([name, commits]) => ({ name, commits })
  );

  return leaderboard;
}

export function lineGraphData(data: FilteredData): LineGraphData[] {
  // check this as written with the asisstance of AI
  const byDate = new Map<string, Record<string, number>>();
  const repoData = data.repositoryData;

  repoData.allCommits.forEach((commit) => {
    const user = commit.value.contributorName;
    const date = new Date(commit.value.timestamp).toISOString().split("T")[0];

    if (!byDate.has(date)) byDate.set(date, {});
    const bucket = byDate.get(date)!;
    bucket[user] = (bucket[user] ?? 0) + 1;
  });

  const dataArray: LineGraphData[] = Array.from(byDate.entries())
    .sort(([a], [b]) => a.localeCompare(b))
    .map(([date, userCommits]) => ({ date, ...userCommits }));

  return dataArray;
}

/**
 *
 * @param data
 * @returns
 */
export function pieChartData(data: FilteredData): PieChartData[] {
  return leaderboardData(data).map((contributor, index) => ({
    user: contributor.name,
    contributions: contributor.commits,
  }));
}
