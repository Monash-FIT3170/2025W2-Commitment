import {
  MetricsData,
  SerializableRepoData,
  FilteredData,
  HighlightStruct,
  LeaderboardData,
  LineGraphData,
  PieChartData,
  HeatMapData,
  Highlights,
  AllMetricsData,
  MetricType,
} from "../imports/api/types";

import {
  leaderboardCommitsPerDay,
  leaderboardLOC,
  leaderboardLOCPerCommit,
  leaderboardTotalCommits,
} from "./leaderboard";
import {
  linegraphCommitsPerDay,
  linegraphLOC,
  linegraphLOCPerCommit,
  linegraphTotalCommits,
} from "./linegraph";
import {
  pieChartCommitsPerDay,
  pieChartLOC,
  pieChartLOCPerCommit,
  pieChartTotalCommits,
} from "./piechart";

import { Meteor } from "meteor/meteor";

// storing a global access unfiltered data here
let unfilteredRepoData = {} as Promise<SerializableRepoData>;

// -------- THIS FUNCTION NEEDS TO BE CALLED FIRST -----------------------
export async function getAllGraphData(
  data: FilteredData,
  selectedMetric: MetricType
): Promise<MetricsData> {
  // set the unfiltered data we will use for all other metrics
  setsUnfilteredData(data.repoUrl);
  // get all the metrics based on the AnalyticsData structure

  let leaderboard: LeaderboardData[];
  let lineGraph: LineGraphData[];
  let pieChart: PieChartData[];
  let heatMap: HeatMapData[];

  switch (selectedMetric) {
    case MetricType.LOC:
      leaderboard = leaderboardLOC(data);
      lineGraph = linegraphLOC(data);
      pieChart = pieChartLOC(data);
      heatMap = heatMapLOC(data);
      break;

    case MetricType.LOC_PER_COMMIT:
      leaderboard = leaderboardLOCPerCommit(data);
      lineGraph = linegraphLOCPerCommit(data);
      pieChart = pieChartLOCPerCommit(data);
      heatMap = heatMapLOCPerCommit(data);
      break;

    case MetricType.COMMITS_PER_DAY:
      leaderboard = leaderboardCommitsPerDay(data);
      lineGraph = linegraphCommitsPerDay(data);
      pieChart = pieChartCommitsPerDay(data);
      heatMap = heatMapCommitsPerDay(data);
      break;

    case MetricType.TOTAL_COMMITS:
      leaderboard = leaderboardTotalCommits(data);
      lineGraph = linegraphTotalCommits(data);
      pieChart = pieChartTotalCommits(data);
      heatMap = heatMapTotalCommits(data);
      break;

    default:
      throw new Error(`Unsupported metric: ${String(selectedMetric)}`);
  }

  return {
    highlights: await returnHighlightData(),

    contributors: {
      leaderboard,
      lineGraph,
      pieChart,
      heatMap,
    },
  };
}

export async function getAllMetrics(repoUrl: string): Promise<AllMetricsData> {
  // does all of this on UNFILTERED DATA
  setsUnfilteredData(repoUrl);
  const unfilteredData = await getUnfilteredData();

  // for each contributor in the unfiltered data, find the metric associated to them:
  const allMetricData: AllMetricsData = {};

  const contributors = getContributors(unfilteredData);

  contributors.forEach((contributor) => {
    allMetricData[contributor] = {
      "Total lines of commit": getTotalCommitsPerContributor(
        unfilteredData,
        contributor
      ),
      LOC: getLOCperContributor(unfilteredData, contributor),
      "LOC/Commit": getLocPerCommitPerContributor(unfilteredData, contributor),
      "Commits Per Day": getCommitPerDayPerContributor(
        unfilteredData,
        contributor
      ),
    };
  });

  return allMetricData;
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
export function getContributors(data: SerializableRepoData): string[] {
  return data.contributors.map((c) => c.value.name);
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
 * HEATMAP FUNCTIONS
 */

export function heatMapLOC(data: FilteredData): HeatMapData[] {
  const repoData = data.repositoryData;
  // Map of date → user → LOC count
  const byDateUser = new Map<string, Record<string, number>>();

  repoData.allCommits.forEach((commit) => {
    const user = commit.value.contributorName;
    const date = new Date(commit.value.timestamp).toISOString().split("T")[0];

    // calculate LOC for this commit
    const locthisCommit = commit.value.fileData.reduce(
      (sum, fileChange) => sum + fileChange.file.contents.split("\n").length,
      0
    );

    if (!byDateUser.has(date)) byDateUser.set(date, {});
    const bucket = byDateUser.get(date)!;
    bucket[user] = (bucket[user] ?? 0) + locthisCommit;
  });

  // convert to heatmapdata[]
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

export function heatMapLOCPerCommit(data: FilteredData): HeatMapData[] {
  const repoData = data.repositoryData;

  // Map of date → user → { totalLOC, commitCount }
  const byDateUser = new Map<
    string,
    Record<string, { totalLOC: number; commitCount: number }>
  >();

  repoData.allCommits.forEach((commit) => {
    const user = commit.value.contributorName;
    const date = new Date(commit.value.timestamp).toISOString().split("T")[0];

    // calculate LOC for this commit
    const locThisCommit = commit.value.fileData.reduce(
      (sum, fileChange) => sum + fileChange.file.contents.split("\n").length,
      0
    );

    if (!byDateUser.has(date)) byDateUser.set(date, {});
    const bucket = byDateUser.get(date)!;

    if (!bucket[user]) bucket[user] = { totalLOC: 0, commitCount: 0 };

    bucket[user].totalLOC += locThisCommit;
    bucket[user].commitCount += 1;
  });

  // Convert to HeatMapData[] with LOC per commit
  const heatMapArray: HeatMapData[] = [];
  byDateUser.forEach((userCounts, date) => {
    for (const [user, { totalLOC, commitCount }] of Object.entries(
      userCounts
    )) {
      heatMapArray.push({
        name: user,
        date,
        count: commitCount > 0 ? totalLOC / commitCount : 0, // LOC per commit
      });
    }
  });

  return heatMapArray;
}

/**
 * Calculates the average number of commits per day for each contributor.
 * @param data The filtered repository data
 * @returns A heat map data structure containing name, date and count for each contributors average number of commits per day.
 */
export function heatMapCommitsPerDay(data: FilteredData): HeatMapData[] {
  const repoData = data.repositoryData;
  const { start, end } = data.dateRange;

  // Calculate number of days in range (inclusive)
  const daysInRange =
    Math.floor((end.getTime() - start.getTime()) / (1000 * 60 * 60 * 24)) + 1;

  // Map of date → user → commit count
  const byDateUser = new Map<string, Record<string, number>>();

  repoData.allCommits.forEach((commit) => {
    const user = commit.value.contributorName;
    const date = new Date(commit.value.timestamp).toISOString().split("T")[0];

    if (!byDateUser.has(date)) byDateUser.set(date, {});
    const bucket = byDateUser.get(date)!;

    bucket[user] = (bucket[user] ?? 0) + 1;
  });

  // Flatten into HeatMapData[] with average per day
  const heatMapArray: HeatMapData[] = [];

  byDateUser.forEach((userCounts, date) => {
    Object.entries(userCounts).forEach(([user, count]) => {
      const avgCount = count / daysInRange; // normalize per day
      heatMapArray.push({
        name: user,
        date,
        count: avgCount,
      });
    });
  });

  return heatMapArray;
}

export function heatMapTotalCommits(data: FilteredData): HeatMapData[] {
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
/**
 * FUNCTIONS FOR HIGHLIGHTS
 */

export async function returnHighlightData(): Promise<Highlights> {
  return {
    totalCommits: await highlightTotalCommits(),
    totalLinesOfCode: await highlightTotalLinesOfCode(),
    numContributors: await numContributors(),
    numBranches: await numBranches(),
  };
}

/**
 *
 * @param data
 * @returns
 */
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

export function getMetricString(): string[] {
  return ["Total No. Commits", "LOC", "LOC/Commit", "Commits Per Day"];
}

//-------------- Metrics PER CONTRIBUTOR FUNCTIONS ----------------

// Function for total lines of commit returning a string

export function getTotalCommitsPerContributor(
  repoData: SerializableRepoData,
  contributorName: string
): number {
  // function that finds all the commits for a contributor and returns the total number of commits as a string
  return repoData.allCommits.filter(
    (commit) => commit.value.contributorName === contributorName
  ).length;
}

export function getLOCperContributor(
  repoData: SerializableRepoData,
  contributorName: string
): number {
  let totalLOC = 0;

  repoData.allCommits.forEach((commit) => {
    if (commit.value.contributorName === contributorName) {
      const locThisCommit = commit.value.fileData.reduce(
        (sum, fileChange) => sum + fileChange.file.contents.split("\n").length,
        0
      );
      totalLOC += locThisCommit;
    }
  });

  return totalLOC;
}

export function getLocPerCommitPerContributor(
  repoData: SerializableRepoData,
  contributorName: string
): number {
  const commits = repoData.allCommits.filter(
    (commit) => commit.value.contributorName === contributorName
  );

  if (commits.length === 0) return 0;

  let totalLOC = 0;

  commits.forEach((commit) => {
    const locThisCommit = commit.value.fileData.reduce((sum, fileChange) => {
      return sum + fileChange.file.contents.split("\n").length;
    }, 0);
    totalLOC += locThisCommit;
  });

  return totalLOC / commits.length; // average LOC per commit
}

export function getCommitPerDayPerContributor(
  repoData: SerializableRepoData,
  contributorName: string
): number {
  // get commits filtered according to contributors
  const commits = repoData.allCommits.filter(
    (commit) => commit.value.contributorName === contributorName
  );
  if (commits.length === 0) return 0;

  // extract commit dates as (YYYY-MM-DD)
  const dates = commits.map((commit) => {
    const date = new Date(commit.value.timestamp);
    return date.toISOString().split("T")[0];
  });

  // count unique days
  const uniqueDays = new Set(dates);
  return commits.length / uniqueDays.size;
}
