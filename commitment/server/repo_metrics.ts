import {
  MetricsData,
  SerializableRepoData,
  FilteredData,
  HighlightStruct,
  LeaderboardData,
  LineGraphData,
  PieChartData,
  HeatMapData,
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
      totalCommits: await highlightTotalCommits(),
      totalLinesOfCode: await highlightTotalLinesOfCode(),
      numContributors: await numContributors(),
      numBranches: await numBranches(),
    },
    contributors: {
      leaderboard: leaderboardData(data),
      lineGraph: lineGraphData(data),
      pieChart: pieChartData(data),
      heatMap: heatMapData(data),
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
export function setsUnfilteredData(repoUrl: string): void {
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
export async function highlightTotalCommits(): Promise<HighlightStruct> {
  const unfilteredData = await getUnfilteredData();

  const totalCommits = getTotalCommits(unfilteredData);

  // Sort commits by timestamp
  const sortedCommits = unfilteredData.allCommits
    .slice()
    .sort(
      (a, b) =>
        new Date(a.value.timestamp).getTime() -
        new Date(b.value.timestamp).getTime()
    );

  // Bucket commits by day
  const commitsByDay: Record<string, number> = {};
  sortedCommits.forEach((commit) => {
    const day = new Date(commit.value.timestamp).toISOString().split("T")[0]; // YYYY-MM-DD
    commitsByDay[day] = (commitsByDay[day] || 0) + 1;
  });

  // Turn buckets into cumulative array
  const days = Object.keys(commitsByDay).sort();
  const commitsCumulative = days.reduce<{ value: number }[]>((acc, day) => {
    const prevTotal = acc.length ? acc[acc.length - 1].value : 0;
    acc.push({ value: prevTotal + commitsByDay[day] });
    return acc;
  }, []);

  // calculate percentage change
  let percentageChange: number = 0;
  if (commitsCumulative.length >= 2) {
    const prev = commitsCumulative[commitsCumulative.length - 2].value;
    const curr = commitsCumulative[commitsCumulative.length - 1].value;

    if (prev !== 0) {
      percentageChange = Math.round(((curr - prev) / prev) * 100);
    }
  }

  return {
    total: totalCommits,
    percentageChange,
    isPositive: percentageChange > 0,
    data: commitsCumulative,
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
 * Count the number of lines in a file contents string.
 * @param fileContents The file contents as a string.
 * @returns The number of lines in the file.
 */
function countLines(fileContents: string): number {
  if (!fileContents) return 0;
  return fileContents.split("\n").length;
}

/**
 * Get the total number of lines of code across all commits in a repository.
 * @param repoData The repository data.
 * @returns The total number of lines of code.
 */
export function getTotalLinesOfCode(repoData: SerializableRepoData): number {
  return repoData.allCommits.reduce<number>((sum, commit) => {
    const commitLines = commit.value.fileData.reduce<number>(
      (fileSum, f) => fileSum + countLines(f.file.contents), // assuming `file.contents` is the raw file text
      0
    );
    return sum + commitLines;
  }, 0);
}

/**
 * Returns the total lines of code(total files changed) in the repository for the highlight card.
 * @param data Filtered Repository Data
 * @returns Highlighted total lines of code information
 */
export async function highlightTotalLinesOfCode(): Promise<HighlightStruct> {
  // number of files changed
  const unfilteredData = await getUnfilteredData();

  const sortedCommits = unfilteredData.allCommits
    .slice()
    .sort(
      (a, b) =>
        new Date(a.value.timestamp).getTime() -
        new Date(b.value.timestamp).getTime()
    );

  // Compute lines of code per commit
  const linesOfCodeOverTime: { value: number }[] = sortedCommits.map(
    (commit) => ({
      value: commit.value.fileData.reduce(
        (sum, fileChange) => sum + fileChange.file.contents.split("\n").length,
        0
      ),
    })
  );

  let percentageChange: number = 0;
  if (linesOfCodeOverTime.length >= 2) {
    const prev = linesOfCodeOverTime[linesOfCodeOverTime.length - 2].value;
    const curr = linesOfCodeOverTime[linesOfCodeOverTime.length - 1].value;

    if (prev !== 0) {
      percentageChange = Math.round(((curr - prev) / prev) * 100);
    }
  }

  return {
    total: getTotalLinesOfCode(unfilteredData),
    percentageChange,
    isPositive: percentageChange > 0,
    data: linesOfCodeOverTime,
  };
}

/**
 * Returns the number of contributors in the repository for the highlight card.
 * @param data Filtered Repository Data
 * @returns Highlighted number of contributors information
 */
export async function numContributors(): Promise<number> {
  const unfilteredData = await getUnfilteredData();
  return unfilteredData.contributors.length;
}

/**
 * Returns the number of branches in the repository for the highlight card.
 * @param data Filtered Repository Data
 * @returns Highlighted number of branches information
 */
export async function numBranches(): Promise<number> {
  const unfilteredData = await getUnfilteredData();
  return unfilteredData.branches.length;
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
  const byDate = new Map<string, Record<string, number>>();
  const repoData = data.repositoryData;

  // Gather all contributors
  const allContributors = new Set<string>();
  repoData.allCommits.forEach((commit) => {
    allContributors.add(commit.value.contributorName);
  });

  // Collect daily LOC
  repoData.allCommits.forEach((commit) => {
    const user = commit.value.contributorName;
    const date = new Date(commit.value.timestamp).toISOString().split("T")[0];

    // LOC snapshot for this commit
    const locThisCommit = commit.value.fileData.reduce(
      (sum, fileChange) => sum + fileChange.file.contents.split("\n").length,
      0
    );
    if (!byDate.has(date)) byDate.set(date, {});
    const bucket = byDate.get(date)!;
    bucket[user] = (bucket[user] ?? 0) + locThisCommit;
  });

  // Sort dates
  const sortedDates = Array.from(byDate.keys()).sort((a, b) =>
    a.localeCompare(b)
  );

  // Cumulative tracker
  const cumulative: Record<string, number> = {};
  allContributors.forEach((c) => (cumulative[c] = 0));

  const dataArray: LineGraphData[] = [];

  sortedDates.forEach((date) => {
    const dailyLOC = byDate.get(date)!;

    // update cumulative totals
    Object.keys(dailyLOC).forEach((user) => {
      cumulative[user] = (cumulative[user] ?? 0) + dailyLOC[user];
    });

    // include *all* contributors, even if they didn’t commit today
    const entry: LineGraphData = { date };
    allContributors.forEach((user) => {
      entry[user] = cumulative[user];
    });

    dataArray.push(entry);
  });

  return dataArray;
}

/**
 * TODO: once we fix piechart
 * @param data
 * @returns
 */
export function pieChartData(data: FilteredData): PieChartData[] {
  return leaderboardData(data).map((contributor, index) => ({
    user: contributor.name,
    contributions: contributor.commits,
  }));
}

export function heatMapData(data: FilteredData): HeatMapData[] {
  const repoData = data.repositoryData;

  // Map of date → user → commit count
  const byDateUser = new Map<string, Record<string, number>>();

  repoData.allCommits.forEach((commit) => {
    const user = commit.value.contributorName;
    const date = new Date(commit.value.timestamp).toISOString().split("T")[0];

    if (!byDateUser.has(date)) byDateUser.set(date, {});
    const bucket = byDateUser.get(date)!;

    bucket[user] = (bucket[user] ?? 0) + 1;
  });

  // Flatten into HeatMapData[]
  const heatMapArray: HeatMapData[] = [];

  byDateUser.forEach((userCounts, date) => {
    Object.entries(userCounts).forEach(([user, count]) => {
      heatMapArray.push({
        name: user,
        date,
        count,
      });
    });
  });

  return heatMapArray;
}
