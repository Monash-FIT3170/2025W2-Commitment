import { Line } from "recharts";
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
import { Meteor } from "meteor/meteor";
import { ZodNumberCheck } from "zod";

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

  console.log(
    "Selected Metric in the Get All Graph Data Function",
    selectedMetric
  );
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
 * LEADERBOARD FUNCTIONS
 */

export function leaderboardTotalCommits(data: FilteredData): LeaderboardData[] {
  const repoData = data.repositoryData;
  const counts: Record<string, number> = {};

  repoData.allCommits.forEach((commit) => {
    const user = commit.value.contributorName;
    counts[user] = (counts[user] ?? 0) + 1;
  });

  const leaderboard: LeaderboardData[] = Object.entries(counts).map(
    ([name, value]) => ({ name, value })
  );

  return leaderboard;
}

export function leaderboardLOC(data: FilteredData): LeaderboardData[] {
  const repoData = data.repositoryData;
  const counts: Record<string, number> = {};

  repoData.allCommits.forEach((commit) => {
    const user = commit.value.contributorName;
    const locThisCommit = commit.value.fileData.reduce(
      (sum, fileChange) => sum + fileChange.file.contents.split("\n").length,
      0
    );

    counts[user] = (counts[user] || 0) + locThisCommit;
  });

  const leaderboard: LeaderboardData[] = Object.entries(counts).map(
    ([name, value]) => ({ name, value })
  );

  return leaderboard;
}

export function leaderboardLOCPerCommit(data: FilteredData): LeaderboardData[] {
  const repoData = data.repositoryData;
  const locCounts: Record<string, number> = {};
  const commitCounts: Record<string, number> = {};

  repoData.allCommits.forEach((commit) => {
    const user = commit.value.contributorName;
    const locThisCommit = commit.value.fileData.reduce(
      (sum, fileChange) => sum + fileChange.file.contents.split("\n").length,
      0
    );

    locCounts[user] = (locCounts[user] || 0) + locThisCommit;
    commitCounts[user] = (commitCounts[user] || 0) + 1;
  });

  const leaderboard: LeaderboardData[] = Object.keys(locCounts).map((user) => {
    const totalLOC = locCounts[user];
    const totalCommits = commitCounts[user];
    const avgLOCPerCommit = totalCommits > 0 ? totalLOC / totalCommits : 0;

    return {
      name: user,
      value: Math.round(avgLOCPerCommit), // nearest whole number
    };
  });

  return leaderboard;
}

/**
 *
 * @param data
 * @returns
 */
export function leaderboardCommitsPerDay(
  data: FilteredData | SerializableRepoData
): LeaderboardData[] {
  // finds the total number of commits per day for a contributor within the date range
  // and then finds the average of these commits to have one value for each contributor
  const repoData = "repositoryData" in data ? data.repositoryData : data;
  // Track total commits and unique days for each contributor
  const contributorStats: Record<
    string,
    { commits: number; days: Set<string> }
  > = {};

  repoData.allCommits.forEach((commit) => {
    const contributor = commit.value.contributorName;
    const date = new Date(commit.value.timestamp).toISOString().split("T")[0];

    if (!contributorStats[contributor]) {
      contributorStats[contributor] = { commits: 0, days: new Set<string>() };
    }

    contributorStats[contributor].commits += 1;
    contributorStats[contributor].days.add(date);
  });

  // Calculate average commits per day for each contributor
  const leaderboard: LeaderboardData[] = Object.entries(contributorStats).map(
    ([contributor, stats]) => {
      const avgCommitsPerDay = stats.commits / stats.days.size;
      return {
        name: contributor,
        value: parseFloat(avgCommitsPerDay.toFixed(2)), // rounding to 2 decimals
      };
    }
  );

  // Sort descending by average commits per day
  leaderboard.sort((a, b) => b.value - a.value);

  return leaderboard;
}

/**
 * LINEGRAPH FUNCTIONS
 */

/**
 * Returns the total number of commits per day for the line graph.
 * @param data Filtered Repository Data
 * @returns Array of LineGraphData objects
 */
export function linegraphTotalCommits(data: FilteredData): LineGraphData[] {
  const repoData = data.repositoryData;

  // Gather all contributors
  const allContributors = new Set<string>();
  repoData.allCommits.forEach((commit) => {
    allContributors.add(commit.value.contributorName);
  });

  // Bucket commits by day per contributor
  const commitsByDay: Record<string, Record<string, number>> = {};
  repoData.allCommits.forEach((commit) => {
    const date = new Date(commit.value.timestamp).toISOString().split("T")[0];
    const contributor = commit.value.contributorName;

    if (!commitsByDay[date]) commitsByDay[date] = {};
    commitsByDay[date][contributor] =
      (commitsByDay[date][contributor] || 0) + 1;
  });

  // Sort dates
  const sortedDates = Object.keys(commitsByDay).sort();

  // Prepare cumulative counts per contributor
  const cumulative: Record<string, number> = {};
  allContributors.forEach((c) => (cumulative[c] = 0));

  // Build LineGraphData array
  const dataArray: LineGraphData[] = [];
  sortedDates.forEach((date) => {
    const dailyCommits = commitsByDay[date];

    // Update cumulative totals
    Object.keys(dailyCommits).forEach((user) => {
      cumulative[user] = (cumulative[user] ?? 0) + dailyCommits[user];
    });

    // Create entry for this date
    const entry: LineGraphData = { date };
    allContributors.forEach((user) => {
      entry[user] = cumulative[user];
    });

    dataArray.push(entry);
  });

  return dataArray;
}

/**
 *
 * @param data
 * @returns
 */
export function linegraphLOC(data: FilteredData): LineGraphData[] {
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
 * Calculates the Commits per day and formats into a line graph strucutre. 
 * @param data The filtered Data passed. 
 * @returns An array of LineGraphData objects representing commits per day.
 */
export function linegraphCommitsPerDay(data: FilteredData): LineGraphData[] {

  const {dateRange, repositoryData} = data; 
  const {allCommits, contributors} = repositoryData; 

  // get contributor names
  const contributorNames = contributors.map(c => c.value.name);

  // Format date as YYYY-MM-DD
  const formatDate = (d:Date) => d.toISOString().split("T")[0];

  // build a map: date -> {contributorName -> count}
  const commitMap: Record<string,  Record<string, number>> = {};

  for (const commitObj of allCommits){
    const commit = commitObj.value; 
    const commitDate = formatDate(new Date(commit.timestamp));

    if (new Date(commit.timestamp) < dateRange.start || new Date(commit.timestamp) > dateRange.end) {
      continue;
    }

    // Initialize commitMap for this date
    if (!commitMap[commitDate]) {
      commitMap[commitDate] = {};
    }
    if (!commitMap[commitDate][commit.contributorName]) {
      commitMap[commitDate][commit.contributorName] = 0;
    }
    commitMap[commitDate][commit.contributorName]++;
  }
  const result: LineGraphData[] = [];
  const current = new Date(dateRange.start); 
  while (current <= dateRange.end){
    const dateStr = formatDate(current); 
    const entry: LineGraphData = {date: dateStr}; 

    for (const name of contributorNames){
      entry[name] = commitMap[dateStr]?.[name] || 0;
    }
    result.push(entry);
    current.setDate(current.getDate() + 1);

  }

  return result;

}

/**
 *
 * @param data
 */
export function linegraphLOCPerCommit(data: FilteredData): LineGraphData[] {
  const byDate = new Map<
    string,
    { loc: Record<string, number>; commits: Record<string, number> }
  >();
  const repoData = data.repositoryData;

  // Gather all contributors
  const allContributors = new Set<string>();
  repoData.allCommits.forEach((commit) => {
    allContributors.add(commit.value.contributorName);
  });

  // Collect daily LOC + commit counts
  repoData.allCommits.forEach((commit) => {
    const user = commit.value.contributorName;
    const date = new Date(commit.value.timestamp).toISOString().split("T")[0];

    // LOC snapshot for this commit
    const locThisCommit = commit.value.fileData.reduce(
      (sum, fileChange) => sum + fileChange.file.contents.split("\n").length,
      0
    );

    if (!byDate.has(date)) {
      byDate.set(date, { loc: {}, commits: {} });
    }
    const bucket = byDate.get(date)!;

    bucket.loc[user] = (bucket.loc[user] ?? 0) + locThisCommit;
    bucket.commits[user] = (bucket.commits[user] ?? 0) + 1;
  });

  // Sort dates
  const sortedDates = Array.from(byDate.keys()).sort((a, b) =>
    a.localeCompare(b)
  );

  // Cumulative trackers
  const cumulativeLOC: Record<string, number> = {};
  const cumulativeCommits: Record<string, number> = {};
  allContributors.forEach((c) => {
    cumulativeLOC[c] = 0;
    cumulativeCommits[c] = 0;
  });

  const dataArray: LineGraphData[] = [];

  sortedDates.forEach((date) => {
    const daily = byDate.get(date)!;

    // update cumulative totals
    Object.keys(daily.loc).forEach((user) => {
      cumulativeLOC[user] += daily.loc[user];
      cumulativeCommits[user] += daily.commits[user];
    });

    // entry for this date = avg LOC per commit
    const entry: LineGraphData = { date };
    allContributors.forEach((user) => {
      const commits = cumulativeCommits[user];
      entry[user] = commits > 0 ? Math.round(cumulativeLOC[user] / commits) : 0;
    });

    dataArray.push(entry);
  });

  return dataArray;
}

/**
 * PIECHART FUNCTIONS
 */

/**
 *
 * @param data
 * @returns
 */
export function pieChartTotalCommits(data: FilteredData): PieChartData[] {
  const repoData = data.repositoryData;
  const counts: Record<string, number> = {};

  repoData.allCommits.forEach((commit) => {
    const user = commit.value.contributorName;
    counts[user] = (counts[user] ?? 0) + 1;
  });

  const pie: PieChartData[] = Object.entries(counts).map(
    ([user, contributions]) => ({ user, contributions })
  );

  return pie;
}

/**
 *
 * @param data
 * @returns
 */
export function pieChartLOC(data: FilteredData): PieChartData[] {
  const repoData = data.repositoryData;
  const counts: Record<string, number> = {};

  repoData.allCommits.forEach((commit) => {
    const user = commit.value.contributorName;
    const locThisCommit = commit.value.fileData.reduce(
      (sum, fileChange) => sum + fileChange.file.contents.split("\n").length,
      0
    );

    counts[user] = (counts[user] || 0) + locThisCommit;
  });

  const pie: PieChartData[] = Object.entries(counts).map(
    ([user, contributions]) => ({ user, contributions })
  );

  return pie;
}

/**
 *
 * @param data
 * @returns
 */
export function pieChartLOCPerCommit(data: FilteredData): PieChartData[] {
  const repoData = data.repositoryData;
  const locCounts: Record<string, number> = {};
  const commitCounts: Record<string, number> = {};

  repoData.allCommits.forEach((commit) => {
    const user = commit.value.contributorName;
    const locThisCommit = commit.value.fileData.reduce(
      (sum, fileChange) => sum + fileChange.file.contents.split("\n").length,
      0
    );

    locCounts[user] = (locCounts[user] || 0) + locThisCommit;
    commitCounts[user] = (commitCounts[user] || 0) + 1;
  });

  const pie: PieChartData[] = Object.keys(locCounts).map((user) => {
    const totalLOC = locCounts[user];
    const totalCommits = commitCounts[user];
    const avgLOCPerCommit = totalCommits > 0 ? totalLOC / totalCommits : 0;

    return {
      user,
      contributions: Math.round(avgLOCPerCommit), // nearest whole number
    };
  });

  return pie;
}

/**
 *
 * @param data
 * @returns
 */
export function pieChartCommitsPerDay(
  data: FilteredData | SerializableRepoData
): PieChartData[] {
  // finds the total number of commits per day for a contributor within the date range
  // and then finds the average of these commits to have one value for each contributor
  const repoData = "repositoryData" in data ? data.repositoryData : data;
  // Track total commits and unique days for each contributor
  const contributorStats: Record<
    string,
    { commits: number; days: Set<string> }
  > = {};

  repoData.allCommits.forEach((commit) => {
    const contributor = commit.value.contributorName;
    const date = new Date(commit.value.timestamp).toISOString().split("T")[0];

    if (!contributorStats[contributor]) {
      contributorStats[contributor] = { commits: 0, days: new Set<string>() };
    }

    contributorStats[contributor].commits += 1;
    contributorStats[contributor].days.add(date);
  });

  // Calculate average commits per day for each contributor
  const pie: PieChartData[] = Object.entries(contributorStats).map(
    ([contributor, stats]) => {
      const avgCommitsPerDay = stats.commits / stats.days.size;
      return {
        user: contributor,
        contributions: Math.round(avgCommitsPerDay), // rounding to 2 decimals
      };
    }
  );

  return pie;
}

/**
 * HEATMAP FUNCTIONS
 */


export function heatMapLOC(data:FilteredData):HeatMapData[] {
  const repoData = data.repositoryData;
  // Map of date → user → LOC count
  const byDateUser = new Map<string, Record<string, number>>();

  repoData.allCommits.forEach((commit) => {
    const user = commit.value.contributorName;
    const date = new Date(commit.value.timestamp).toISOString().split("T")[0];

    // calculate LOC for this commit
    const locthisCommit = commit.value.fileData.reduce(
      (sum, fileChange) => sum + fileChange.file.contents.split("\n").length, 0 
    ); 

    if(!byDateUser.has(date)) byDateUser.set(date, {}); 
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
  const byDateUser = new Map<string, Record<string, { totalLOC: number; commitCount: number }>>();

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
    for (const [user, { totalLOC, commitCount }] of Object.entries(userCounts)) {
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
  const locArray = locData(repoData); // LineGraphData[] (cumulative LOC over time)

  if (locArray.length === 0) return 0;

  const lastEntry = locArray[locArray.length - 1]; // final cumulative snapshot

  return lastEntry[contributorName]
    ? (lastEntry[contributorName] as number)
    : 0;
}

export function getLocPerCommitPerContributor(
  repoData: SerializableRepoData,
  contributorName: string
): number {
  return 0; // until implemented
}

export function getCommitPerDayPerContributor(
  repoData: SerializableRepoData,
  contributorName: string
): number {
  const commitsPerDayData = commitsPerDay(repoData);
  return commitsPerDayData.find((c) => c.name === contributorName)?.value ?? 0;
}
