// server/repo_metrics.ts

import { get } from "http";
import { RepositoryData, CommitData, SerializableRepoData, FilteredData } from "../imports/api/types";
import { serializeRepoData } from "/imports/api/serialisation";
import { Meteor } from "meteor/meteor";

// ------------------ DATA STRUCTURES TO USE ------------------------
interface HighlightStruct{
  total: number;
  percentageChange: number;
  isPositive: boolean;
  data: { value: number }[];
}

interface LeaderboardData{
  name: string; 
  commits: number; 
}

interface LineGraphData{
  date: string; "YYYY-MM-DD"
  [contributor: string]: number; // e.g. { Alice: 120, Bob: 95 }
}

interface PieChartData{
  user: string; 
  contributions: number; 
  fill: string; // color
}

// storing a global access unfiltered data here
let unfilteredRepoData = {} as Promise<SerializableRepoData>;

// -------- THIS FUNCTION NEEDS TO BE CALLED FIRST ----- make it a meteor call method ? 
export function getAllMetrics(data:FilteredData){
  // set the unfiltered data we will use for all other metrics
  setsUnfilteredData(data.repoUrl);
}

/**
 * Fetch unfiltered repository data from the database.
 * @param repoUrl The URL of the repository.
 * @returns A promise that resolves to the unfiltered repository data.
 */
export function setsUnfilteredData(repoUrl:string){
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
/**
 * Get the percentage change of commits in a repository.
 * @param startDate The start date for the comparison.
 * @param repoUrl The URL of the repository.
 * @param data The filtered repository data.
 * @returns The percentage change of commits.
 */
async function percentageCommitChange(startDate:Date, repoUrl:string, data: SerializableRepoData): Promise<number> {

  // get original data
  const unfilteredData = await getUnfilteredData();

  // get the length of commits from the beginning up until the start date
  const prevCommits = unfilteredData.allCommits.filter((c) => new Date(c.value.timestamp).getTime() < startDate.getTime()).length;
  // get the length of commits in the filtered data 
  const currentCommits = data.allCommits.length;

  // calculate the percentage change 
  const pChange = ((currentCommits - prevCommits) / (prevCommits)) * 100;

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
export async function highlightTotalCommits(data: FilteredData): Promise<HighlightStruct> {
  const repoData = data.repositoryData
  const totalCommits = getTotalCommits(repoData);

  // calculate percentage change
  const pChange = await percentageCommitChange(data.dateRange.start, data.repoUrl, repoData);
  

  const commitsData = repoData.allCommits.map(({ value }) => ({ value: 1 })); // each commit counts as 1

  return {
    total: totalCommits,
    percentageChange: pChange,
    isPositive: pChange > 0,
    data: commitsData
  };
}

/**
 * Get the total number of files changed in a repository.
 * @param repoData The repository data.
 * @returns The total number of files changed.
 */
export function getTotalFilesChanged(repoData: SerializableRepoData):number {
  return repoData.allCommits.reduce<number>((sum, commit) => {
    const numFiles =commit.value.fileData.length;
    return numFiles + sum;
  }, 0);
}

/**
 * Returns the total lines of code(total files changed) in the repository for the highlight card. 
 * @param data Filtered Repository Data
 * @returns Highlighted total lines of code information
 */
export async function highlightTotalLinesOfCode(data: FilteredData):Promise<HighlightStruct>{
  const repoData = data.repositoryData;
  
  // number of files changed 
  const currentFiles = getTotalFilesChanged(repoData);
  const unfilteredData = await getUnfilteredData(); 

  const prevFiles = getTotalFilesChanged({
    ...unfilteredData, 
    allCommits: unfilteredData.allCommits.filter(
      (c) => new Date(c.value.timestamp).getTime() < data.dateRange.start.getTime()
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

  const leaderboard: LeaderboardData[] = Object.entries(counts).map(([name, commits]) => ({ name, commits }));
  
  return leaderboard;
}

// ------------------------- COMMENTING OUT ORIGINAL REPO_METRICS INFO ---------

// /**
//  * Count “LOC changed” for a commit.
//  * NOTE: This shouldn't be final implementation of this method.
//  * If we later store git diffs, we need to replace this.
//  */
// export function getLOCFromCommit(commit: CommitData): number {
//   if (!commit.fileData) return 0;
//   return commit.fileData.reduce((acc, fileChange) => {
//     const content = fileChange.file?.contents || "";
//     return acc + content.split("\n").length;
//   }, 0);
// }




// /**
//  * LOC line dataset (graph-ready)
//  * {
//  *   title: "Lines of Codes Changed Over Time",
//  *   data: [
//  *     { date: "YYYY-MM-DD", Alice: 120, Bob: 90, ... },
//  *     ...
//  *   ]
//  * }
//  */
// export function getLocLineData(data: RepositoryData): {
//   title: string;
//   data: { [key: string]: number | string }[];
// } {
//   // date -> { userName -> totalLocOnThatDate }
//   const byDate = new Map<string, Record<string, number>>();

//   data.allCommits.forEach((commit) => {
//     const user = commit.contributorName;
//     const loc = getLOCFromCommit(commit);
//     const date = commit.timestamp.toISOString().split("T")[0];

//     if (!byDate.has(date)) byDate.set(date, {});
//     const bucket = byDate.get(date)!;
//     bucket[user] = (bucket[user] ?? 0) + loc;
//   });

//   const dataArray = Array.from(byDate.entries())
//     .sort(([a], [b]) => a.localeCompare(b))
//     .map(([date, userLocs]) => ({ date, ...userLocs }));

//   return {
//     title: "Lines of Codes Changed Over Time",
//     data: dataArray,
//   };
// }

// /**
//  * Total commits by contributor (graph-ready)
//  * {
//  *   title: "All Contributor Commits",
//  *   data: [{ name: "Alice", commits: 100 }, ...]
//  * }
//  */
// export function getAllContributorsCommits(data: RepositoryData): {
//   title: string;
//   data: { name: string; commits: number }[];
// } {
//   const counts = new Map<string, number>();

//   data.allCommits.forEach((commit) => {
//     const user = commit.contributorName;
//     counts.set(user, (counts.get(user) ?? 0) + 1);
//   });

//   const list = Array.from(counts.entries()).map(([name, commits]) => ({ name, commits }));
//   return { title: "All Contributor Commits", data: list };
// }

// /** Total LOC by contributor (graph-ready list): [{ name, value }] */
// export function getTotalLocData(data: RepositoryData): { name: string; value: number }[] {
//   const locs = new Map<string, number>();

//   data.allCommits.forEach((commit) => {
//     const user = commit.contributorName;
//     const loc = getLOCFromCommit(commit);
//     locs.set(user, (locs.get(user) ?? 0) + loc);
//   });

//   return Array.from(locs.entries()).map(([name, value]) => ({ name, value }));
// }

// /** Type and registry for metric-lookup by name */
// // export type MetricFn = (data: RepositoryData) => any;

// // export const metricsFunctions = new Map<string, MetricFn>([
// //   ["branches", getBranches],
// //   ["contributors", getContributors],
// //   ["users", getUsers],
// //   ["locLineData", getLocLineData],
// //   ["allContributorCommits", getAllContributorsCommits],
// //   ["totalLocData", getTotalLocData],
// // ]);

// // /** Convenience bundle (optional) */
// // export function getAllMetricsBundle(data: RepositoryData) {
// //   return {
// //     branches: getBranches(data),
// //     contributors: getContributors(data),
// //     users: getUsers(data),
// //     locLineData: getLocLineData(data),
// //     allContributorCommits: getAllContributorsCommits(data),
// //     totalLocData: getTotalLocData(data),
// //   };
// // }


// /** Users who actually committed (derived from commits’ contributorName field) */
// export function getUsers(data: RepositoryData): string[] {
//   const set = new Set<string>();
//   data.allCommits.forEach((c) => set.add(c.contributorName));
//   return Array.from(set);
// }