import { DateRange } from "react-day-picker";

export type RepositoryData = Readonly<{
  name: string;
  branches: BranchData[];
  allCommits: Map<string, CommitData>;
  contributors: Map<string, ContributorData>;
}>;

export type BranchData = Readonly<{
  branchName: string;
  commitHashes: string[];
}>;

// make a kind of commit where you hazve a snapshot of all contributors per line
export type CommitData = Readonly<{
  commitHash: string;
  commitTitle: string;
  contributorName: string;
  description: string;
  timestamp: string;
  fileData: FileChanges[];
}>;

export type FileChanges = Readonly<{
  filepath: string;
  oldFilePath: string;
  char: ChangeType;
  likeness: number;
  newLines: number;
  deletedLines: number;
  diff: string[];
}>;

export type ChangeType = "A" | "M" | "D" | "R" | "C";

export type ContributorData = Readonly<{
  name: string;
  emails: string[];
}>;

export type Maybe<T> = T | null;

export type SerialisableMapObject<K, V> = {
  key: K;
  value: V;
};

export type SerializableRepoData = Readonly<{
  name: string;
  branches: BranchData[];
  allCommits: SerialisableMapObject<string, CommitData>[]; // Map converted to a list of objects
  contributors: SerialisableMapObject<string, ContributorData>[]; // Map converted to a list of objects
}>;

export interface FilteredData {
  repoUrl: string;
  dateRange: {
    start: Date;
    end: Date;
  };
  repositoryData: SerializableRepoData;
}

export type AliasEmail = {
  username: string;
  email: string | null;
};

export type UserScalingSummary = {
  name: string;
  aliases: AliasEmail[];
  finalGrade: number | null; // may be missing -> if there is a grading sheet, this must be populated
  scale: number; // always present
};

/**
 * Data Structures for Analytics View
 */
export interface Metadata {
  repoUrl: string;
  repoName: string;
  branches: string[];
  contributors: string[];
  dateRange: DateRange;
}

export interface HighlightStruct {
  total: number;
  percentageChange: number;
  isPositive: boolean;
  data: { value: number }[];
}

export interface LeaderboardData {
  name: string;
  value: number;
}

export interface LineGraphData {
  date: string;
  [contributor: string]: number | string; // e.g. { Alice: 120, Bob: 95 }
}

export interface PieChartData {
  user: string;
  contributions: number;
  // fill: string; // color
}

export type HeatMapData = {
  name: string;
  date: string;
  count: number;
};

export type Highlights = {
  totalCommits: HighlightStruct;
  totalLinesOfCode: HighlightStruct;
  numContributors: number;
  numBranches: number;
};

export interface Leaderboard {
  data: LeaderboardData[];
  title: string;
  xAxisLabel: string;
}

export interface LineGraph {
  data: LineGraphData[];
  title: string;
  xAxisLabel: string;
  yAxisLabel: string;
}

export interface Heatmap {
  data: HeatMapData[];
  title: string;
}

export interface PieChart {
  data: PieChartData[];
  title: string;
}

export interface Selections {
  selectedBranch: string;
  selectedContributors: string[];
  selectedMetrics: string;
  selectedDateRange: DateRange;
}

export interface AnalyticsData {
  metadata: Metadata;
  selections: Selections;
  metrics: MetricsData;
}

export enum MetricType {
  LOC = "LOC",
  LOC_PER_COMMIT = "LOC Per Commit",
  COMMITS_PER_DAY = "Commits per Day",
  TOTAL_COMMITS = "Total No. Commits",
}

// Helper: turn enum values into a string array
export const metricNames: string[] = Object.values(MetricType);

// add a type for getAllMetrics.

export type ContributorValueWithAliases = {
  name: string;
  emails: string[];
  aliases: { username: string; email: string | null }[];
};

export type UnmappedContributor = {
  name: string;
  rawIdentifiers: string[];
};

export type AllMetricsData = {
  contributorName: string;
  metrics: {
    "Total No. Commits": number;
    LOC: number;
    "LOC Per Commit": number;
    "Commits Per Day": number;
  };
};

export type ContributorMetrics = {
  contributorName: string;
  metrics: Record<MetricType, number>;
};

export type ContributorScaledMetric = {
  metric: MetricType;
  value: number;
  percentile: number;
};

export type ContributorScaledData = {
  contributor: ContributorData;
  scaledMetric: ContributorScaledMetric;
};

export type RepoMetricDistribution = {
  metric: MetricType;
  min: number;
  Q1: number;
  median: number;
  Q3: number;
  max: number;
  mean: number;
};

// for contributor metric scaling graph
export type ScalingDistributionResult = {
  contributors: ContributorScaledData[];
  repoDistributions: RepoMetricDistribution;
};

export type ScalingDistributionGraph = {
  data: ScalingDistributionResult;
  title: string;
};

/**
 * Example of Scaling Distribution Result:
 * {
 *  contributors: [
 *   {
 *      contributorName: "Alice",
 *      scaledMetrics: [
 *        { metric: "LOC", value: 1500, percentile: 85},
 *      ... other contributors
 *  repoDistributions: [
 *  { metric: "LOC", min: 100, Q1: 500, median: 1000, Q3: 2000, max: 5000, mean: 1200},
 * ]
 */

export interface MetricsData {
  highlights: Highlights;
  contributors: {
    leaderboard: Leaderboard;
    lineGraph: LineGraph;
    pieChart: PieChart;
    heatMap: Heatmap;
    scalingDistribution: ScalingDistributionGraph;
  };
}
