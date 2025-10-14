import {
  FilteredData,
  LeaderboardData,
  SerializableRepoData,
} from "@api/types";

import {
  getAllContributorsCommitCounts,
  getLinesOfCodeFromCommit
} from "./helper_functions"

/**
 * LEADERBOARD FUNCTIONS
 */

export const leaderboardTotalCommits = (data: FilteredData): LeaderboardData[] => 
  getAllContributorsCommitCounts(data.repositoryData)
    .sort((a, b) => b.value - a.value).slice(0, 10)

export function leaderboardLOC(data: FilteredData): LeaderboardData[] {
  const repoData = data.repositoryData;
  const counts: Record<string, number> = {};

  repoData.allCommits.forEach((p) => {
    const commit = p.value
    const user = commit.contributorName;
    const locThisCommit = getLinesOfCodeFromCommit(commit)
    counts[user] = (counts[user] || 0) + locThisCommit;
  });

  const leaderboard: LeaderboardData[] = Object.entries(counts).map(
    ([name, value]) => ({ name, value })
  );

  return leaderboard.sort((a, b) => b.value - a.value).slice(0, 10);
}

export function leaderboardLOCPerCommit(data: FilteredData): LeaderboardData[] {
  const repoData = data.repositoryData;
  const locCounts: Record<string, number> = {};
  const commitCounts: Record<string, number> = {};

  repoData.allCommits.forEach((p) => {
    const commit = p.value
    const user = commit.contributorName;
    const locThisCommit = getLinesOfCodeFromCommit(commit)

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

  return leaderboard.sort((a, b) => b.value - a.value).slice(0, 10);
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

  return leaderboard.sort((a, b) => b.value - a.value).slice(0, 10);
}
