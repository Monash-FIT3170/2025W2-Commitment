import { Meteor } from "meteor/meteor";

import {
  MetricsData,
  SerializableRepoData,
  CommitData,
  FilteredData,
  HighlightStruct,
  LeaderboardData,
  LineGraphData,
  PieChartData,
  HeatMapData,
  Highlights,
  AllMetricsData,
  MetricType,
  Heatmap,
  PieChart,
  LineGraph,
  Leaderboard,
} from "../imports/api/types";

import {
  getContributors,
  getTotalCommits,
  getTotalLinesOfCode,
  getTotalCommitsPerContributor,
  getLOCperContributor,
  getLocPerCommitPerContributor,
  getCommitPerDayPerContributor,
  getNumberOfContributors,
  getTotalBranches,
} from "./helper_functions";

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
import {
  heatMapCommitsPerDay,
  heatMapLOC,
  heatMapLOCPerCommit,
  heatMapTotalCommits,
} from "./heatmap";

// -------- THIS FUNCTION NEEDS TO BE CALLED FIRST -----------------------
export async function getAllGraphData(
  data: FilteredData,
  selectedMetric: MetricType
): Promise<MetricsData> {
  // get all the metrics based on the AnalyticsData structure

  let leaderboard: Leaderboard;
  let lineGraph: LineGraph;
  let pieChart: PieChart;
  let heatMap: Heatmap;

  switch (selectedMetric) {
    case MetricType.LOC:
      leaderboard = {
        data: leaderboardLOC(data),
        title: "Top Contributors by Lines of Code",
        xAxisLabel: "Lines of Code",
      };
      lineGraph = {
        data: linegraphLOC(data),
        title: "Lines of Code Over Time",
        xAxisLabel: "Date",
        yAxisLabel: "Lines of Code",
      };
      pieChart = {
        data: pieChartLOC(data),
        title: "Distribution of Lines of Code",
      };
      heatMap = {
        data: heatMapLOC(data),
        title: "Commit Activity (LOC)",
      };
      break;

    case MetricType.LOC_PER_COMMIT:
      leaderboard = {
        data: leaderboardLOCPerCommit(data),
        title: "Top Contributors by LOC per Commit",
        xAxisLabel: "Lines of Code / Commit",
      };
      lineGraph = {
        data: linegraphLOCPerCommit(data),
        title: "LOC per Commit Over Time",
        xAxisLabel: "Date",
        yAxisLabel: "LOC per Commit",
      };
      pieChart = {
        data: pieChartLOCPerCommit(data),
        title: "Distribution of LOC per Commit",
      };
      heatMap = {
        data: heatMapLOCPerCommit(data),
        title: "Commit Activity (LOC per Commit)",
      };
      break;

    case MetricType.COMMITS_PER_DAY:
      leaderboard = {
        data: leaderboardCommitsPerDay(data),
        title: "Top Contributors by Commits per Day",
        xAxisLabel: "Commits / Day",
      };
      lineGraph = {
        data: linegraphCommitsPerDay(data),
        title: "Commits per Day Over Time",
        xAxisLabel: "Date",
        yAxisLabel: "Commits per Day",
      };
      pieChart = {
        data: pieChartCommitsPerDay(data),
        title: "Distribution of Commits per Day",
      };
      heatMap = {
        data: heatMapTotalCommits(data),
        title: "Commit Activity (Commits per Day)",
      };
      break;

    case MetricType.TOTAL_COMMITS:
      leaderboard = {
        data: leaderboardTotalCommits(data),
        title: "Top Contributors by Total Commits",
        xAxisLabel: "Total Commits",
      };
      lineGraph = {
        data: linegraphTotalCommits(data),
        title: "Total Commits Over Time",
        xAxisLabel: "Date",
        yAxisLabel: "Total Commits",
      };
      pieChart = {
        data: pieChartTotalCommits(data),
        title: "Distribution of Total Commits",
      };
      heatMap = {
        data: heatMapTotalCommits(data),
        title: "Commit Activity (Total Commits)",
      };
      break;

    default:
      throw new Error(`Unsupported metric: ${String(selectedMetric)}`);
  }

  return {
    highlights: returnHighlightData(data.repositoryData),
    contributors: {
      leaderboard,
      lineGraph,
      pieChart,
      heatMap,
    },
  };
}

export const getAllMetrics = (filteredData: SerializableRepoData): AllMetricsData => {
  // does all of this on FILTERED DATA

  // for each contributor in the unfiltered data, find the metric associated to them:
  const allMetricData: AllMetricsData = {};

  const contributors = getContributors(filteredData);

  contributors.forEach((contributor) => {
    allMetricData[contributor] = {
      "Total No. Commits": getTotalCommitsPerContributor(filteredData, contributor),
      LOC: getLOCperContributor(filteredData, contributor),
      "LOC Per Commit": getLocPerCommitPerContributor(filteredData, contributor),
      "Commits Per Day": getCommitPerDayPerContributor(filteredData, contributor),
    };
  });

  return allMetricData;
};

/**
 * Get all metrics from provided repository data (useful for alias-mapped data)
 * @param repoData Repository data to calculate metrics from
 * @returns AllMetricsData object with metrics for each contributor
 */
export const getAllMetricsFromData = (repoData: SerializableRepoData): AllMetricsData => {
  // for each contributor in the provided data, find the metric associated to them:
  const allMetricData: AllMetricsData = {};

  const contributors = getContributors(repoData);

  contributors.forEach((contributor) => {
    allMetricData[contributor] = {
      "Total No. Commits": getTotalCommitsPerContributor(repoData, contributor),
      LOC: getLOCperContributor(repoData, contributor),
      "LOC Per Commit": getLocPerCommitPerContributor(repoData, contributor),
      "Commits Per Day": getCommitPerDayPerContributor(repoData, contributor),
    };
  });

  return allMetricData;
};

/**
 * FUNCTIONS FOR HIGHLIGHTS
 */
export const returnHighlightData = (data: SerializableRepoData): Highlights =>
  ({
    totalCommits: highlightTotalCommits(data),
    totalLinesOfCode: highlightTotalLinesOfCode(data),
    numContributors: getNumberOfContributors(data),
    numBranches: getTotalBranches(data),
  } as Highlights);

/**
 * Returns the total commits in a repository for a highlight card.
 * @param data Filtered Repository Data
 * @returns Highlighted total commits information
 */
export const highlightTotalCommits = (unfilteredData: SerializableRepoData): HighlightStruct => {
  const totalCommits = getTotalCommits(unfilteredData);

  // Sort commits by timestamp
  const sortedCommits = unfilteredData.allCommits
    .slice()
    .sort((a, b) => new Date(a.value.timestamp).getTime() - new Date(b.value.timestamp).getTime());

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
};

/**
 * Returns the total lines of code(total files changed) in the repository for the highlight card.
 * @param data Filtered Repository Data
 * @returns Highlighted total lines of code information
 */
export const highlightTotalLinesOfCode = (
  unfilteredData: SerializableRepoData
): HighlightStruct => {
  // number of files changed
  const sortedCommits = unfilteredData.allCommits
    .slice()
    .sort((a, b) => new Date(a.value.timestamp).getTime() - new Date(b.value.timestamp).getTime());

  // Compute lines of code per commit
  const linesOfCodeOverTime: { value: number }[] = sortedCommits.map((commit) => ({
    value: commit.value.fileData.reduce(
      (sum, fileChange) => sum + fileChange.newLines - fileChange.deletedLines,
      0
    ),
  }));

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
};

export function getMetricString(): string[] {
  return ["Total No. Commits", "LOC", "LOC Per Commit", "Commits Per Day"];
}
