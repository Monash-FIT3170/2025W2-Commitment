// server/metrics.ts

// Importing required types to enforce structure of input data
import { RepositoryData, CommitData } from "./commitment_api/types";

// Type definitions for the output structs 

// LOC over time (for line graph)
type TimeLOCEntry = { timestamp: Date; locChanged: number };

// commit time tracking (for heatmap)
type CommitFrequencyEntry = { timestamp: Date };

// summary per user (for pie charts)
type UserStats = { commits: number; loc: number };

// placeholder type for scoring results (for race chart)
type ScalingData = { score: number };

// utility func

// calculates the Lines of Code (LOC) changed in a single commit
function getLOCFromCommit(commit: CommitData): number {
  // if theres no file data, LOC is 0
  if (!commit.fileData) return 0;

  // looping through all the file changes in this commit and sum line counts
  return commit.fileData.reduce((acc, fileChange) => {
    const content = fileChange.file?.contents || "";             // get the file content or fallback to empty
    return acc + content.split("\n").length;                     // count lines and add to accumulator
  }, 0); // initial accumulator value is 0
}

// line Graph (LOC over time)

export function getLOCOverTimePerUser(data: RepositoryData): Map<string, TimeLOCEntry[]> {
  const result = new Map<string, TimeLOCEntry[]>();              // Map: username -> list of time-LOC entries

  // go through each commit in the repo
  data.allCommits.forEach((commit) => {
    const user = commit.contributorName;                         // get contributor name
    const loc = getLOCFromCommit(commit);                        // calc LOC for this commit

    // initialise array if this user hasnt been seen yet
    if (!result.has(user)) result.set(user, []);

    // add LOC entry with timestamp
    result.get(user)!.push({ timestamp: new Date(commit.timestamp), locChanged: loc });
  });

  return result; // return the map of user -> LOC-over-time data
}

// Heatmap - Commit frequency over time

export function getCommitFrequencyPerUser(data: RepositoryData): Map<string, CommitFrequencyEntry[]> {
  const result = new Map<string, CommitFrequencyEntry[]>();      // Map: username -> list of timestamps

  data.allCommits.forEach((commit) => {
    const user = commit.contributorName;         // Get contributor's name

    // initialise array for user if not seen before
    if (!result.has(user)) result.set(user, []);

    // add timestamp of this commit
    result.get(user)!.push({ timestamp: new Date(commit.timestamp) });
  });

  return result; // return map of user -> list of commit times
}

// Pie Chart - total commits and LOC

export function getTotalCommitsAndLOCPerUser(data: RepositoryData): Map<string, UserStats> {
  const result = new Map<string, UserStats>();                   // Map: username -> { total commits, total LOC }

  data.allCommits.forEach((commit) => {
    const user = commit.contributorName;                         // contributors name
    const loc = getLOCFromCommit(commit);                        // LOC for this commit

    // if this is users first commit initialise their stats
    if (!result.has(user)) result.set(user, { commits: 0, loc: 0 });

    // update users stats
    const stats = result.get(user)!;
    stats.commits += 1;                                          // increment commit count
    stats.loc += loc;                                            // add LOC
  });

  return result; // return user -> stats map
}

// Race Chart - aggregate scaling

export function getAggregateScalingPerUser(data: RepositoryData): Map<string, ScalingData> {
  const result = new Map<string, ScalingData>();                 // Map: username -> scaling score

  // first, get total commits and LOC per user
  const userStats = getTotalCommitsAndLOCPerUser(data);

  // calculate score for each user
  userStats.forEach((stats, user) => {
    // using a placeholder formula: score = commits + (LOC / 100)
    result.set(user, { score: stats.commits + stats.loc / 100 });
  });

  return result; // Return user -> aggregate score
}

// Metric Function Map

// This map lets you call a metric by its name string (modular dispatch)
export const metricFunctionMap = new Map<string, (data: RepositoryData) => any>([
  ["getLOCOverTimePerUser", getLOCOverTimePerUser],
  ["getCommitFrequencyPerUser", getCommitFrequencyPerUser],
  ["getTotalCommitsAndLOCPerUser", getTotalCommitsAndLOCPerUser],
  ["getAggregateScalingPerUser", getAggregateScalingPerUser]
]);
