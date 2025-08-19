// server/repo_metrics.ts

import { RepositoryData, CommitData } from "./commitment_api/types";

/**
 * Count “LOC changed” for a commit.
 * NOTE: This shouldn't be final implementation of this method.
 * If we later store git diffs, we need to replace this.
 */
function getLOCFromCommit(commit: CommitData): number {
  if (!commit.fileData) return 0;
  return commit.fileData.reduce((acc, fileChange) => {
    const content = fileChange.file?.contents || "";
    return acc + content.split("\n").length;
  }, 0);
}

/** Branch names */
export function getBranches(data: RepositoryData): string[] {
  return data.branches.map(b => b.branchName);
}

/** Contributors (from RepositoryData.contributors map) */
export function getContributors(data: RepositoryData): string[] {
  return Array.from(data.contributors.values()).map(c => c.name);
}

/** Users who actually committed (derived from commits’ contributorName field) */
export function getUsers(data: RepositoryData): string[] {
  const set = new Set<string>();
  data.allCommits.forEach(c => set.add(c.contributorName));
  return Array.from(set);
}

/**
 * LOC line dataset (graph-ready)
 * {
 *   title: "Lines of Codes Changed Over Time",
 *   data: [
 *     { date: "YYYY-MM-DD", Alice: 120, Bob: 90, ... },
 *     ...
 *   ]
 * }
 */
export function getLocLineData(data: RepositoryData): {
  title: string;
  data: { [key: string]: number | string }[];
} {
  // date -> { userName -> totalLocOnThatDate }
  const byDate = new Map<string, Record<string, number>>();

  data.allCommits.forEach(commit => {
    const user = commit.contributorName;
    const loc = getLOCFromCommit(commit);
    const date = commit.timestamp.toISOString().split("T")[0];

    if (!byDate.has(date)) byDate.set(date, {});
    const bucket = byDate.get(date)!;
    bucket[user] = (bucket[user] ?? 0) + loc;
  });

  const dataArray = Array.from(byDate.entries())
    .sort(([a], [b]) => a.localeCompare(b))
    .map(([date, userLocs]) => ({ date, ...userLocs }));

  return {
    title: "Lines of Codes Changed Over Time",
    data: dataArray
  };
}

/**
 * Total commits by contributor (graph-ready)
 * {
 *   title: "All Contributor Commits",
 *   data: [{ name: "Alice", commits: 100 }, ...]
 * }
 */
export function getAllContributorsCommits(data: RepositoryData): {
  title: string;
  data: { name: string; commits: number }[];
} {
  const counts = new Map<string, number>();

  data.allCommits.forEach(commit => {
    const user = commit.contributorName;
    counts.set(user, (counts.get(user) ?? 0) + 1);
  });

  const list = Array.from(counts.entries()).map(([name, commits]) => ({ name, commits }));
  return { title: "All Contributor Commits", data: list };
}

/** Total LOC by contributor (graph-ready list): [{ name, value }] */
export function getTotalLocData(data: RepositoryData): { name: string; value: number }[] {
  const locs = new Map<string, number>();

  data.allCommits.forEach(commit => {
    const user = commit.contributorName;
    const loc = getLOCFromCommit(commit);
    locs.set(user, (locs.get(user) ?? 0) + loc);
  });

  return Array.from(locs.entries()).map(([name, value]) => ({ name, value }));
}

/** Type and registry for metric-lookup by name */
export type MetricFn = (data: RepositoryData) => any;

export const metricsFunctions = new Map<string, MetricFn>([
  ["branches",               getBranches],
  ["contributors",           getContributors],
  ["users",                  getUsers],
  ["locLineData",            getLocLineData],
  ["allContributorCommits",  getAllContributorsCommits],
  ["totalLocData",           getTotalLocData],
]);

/** Convenience bundle (optional) */
export function getAllMetricsBundle(data: RepositoryData) {
  return {
    branches: getBranches(data),
    contributors: getContributors(data),
    users: getUsers(data),
    locLineData: getLocLineData(data),
    allContributorCommits: getAllContributorsCommits(data),
    totalLocData: getTotalLocData(data),
  };
}
