import {
  FilteredData,
  PieChartData,
  SerializableRepoData,
} from "../imports/api/types";

import {
  getLinesOfCodeFromCommit
} from "./helper_functions"

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

  repoData.allCommits.forEach((p) => {
    const commit = p.value
    const user = commit.contributorName;
    const locThisCommit = getLinesOfCodeFromCommit(commit)

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

  repoData.allCommits.forEach((p) => {
    const commit = p.value
    const user = commit.contributorName;
    const locThisCommit = getLinesOfCodeFromCommit(commit)

    locCounts[user] = (locCounts[user] || 0) + locThisCommit;
    commitCounts[user] = (commitCounts[user] || 0) + 1;
  })

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
