import { DateRange } from "react-day-picker";

export type RepositoryData = Readonly<{
  name: string;
  branches: BranchData[];
  allCommits: Map<string, CommitData>;
  contributors: Map<string, ContributorData>;
}>;

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
  timestamp: Date;
  fileData: FileChanges[];
}>;

export type ContributorData = Readonly<{
  name: string;
  emails: string[];
}>;

export type FileChanges = Readonly<{
  file: FileContents;
  changes: ChangeData;
}>;

export type FileContents = Readonly<{
  contents: string;
  filepath: string;
}>;

export type ChangeData = Readonly<{
  char: ChangeType;
  extra: null | ModifyData | RenameData | CopyData;
}>;

export type ChangeType = "A" | "M" | "D" | "R" | "C";
// create diff section and greatly improve efficiency of storage by hashing FileContents so no repeated information is stashed in the DB
export type ModifyData = Readonly<{ previousFile: FileContents }>;
export type RenameData = Readonly<{ oldFilePath: string; likeness: number }>;
export type CopyData = Readonly<{ oldFilePath: string; copyLikeness: number }>;

export type AliasEmail = {
  username: string;
  email: string;
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
  dateRange: DateRange
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

}
export interface MetricsData {
  highlights: Highlights;
  contributors: {
    leaderboard: LeaderboardData[];
    lineGraph: LineGraphData[];
    pieChart: PieChartData[];
    heatMap: HeatMapData[];
  };
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
  metricNames: string[]; 
}

// add a type for getAllMetrics. 

export interface AllMetricsData{
  [contributorName: string]: {
    "Total lines of commit": number;
    "LOC": number;
    "LOC/Commit": number;
    "Commits Per Day": number;
  };
}