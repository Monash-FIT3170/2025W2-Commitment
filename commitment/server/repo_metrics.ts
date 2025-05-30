// server/repo_metrics.ts

// Latest version of metric retrieval implementation
// Combined functionality of metrics.ts and metrics_transformer.ts to make it mroe compact 
// and convenient for front end calling

import { RepositoryData, CommitData } from "./commitment_api/types";

// utility that calculates LOC per commit
function getLOCFromCommit(commit: CommitData): number {
  if (!commit.fileData) return 0;
  return commit.fileData.reduce((acc, fileChange) => {
    const content = fileChange.file?.contents || "";
    return acc + content.split("\n").length;
  }, 0);
}

// get branch names
export function getBranches(data: RepositoryData): string[] {
  return data.branches.map(branch => branch.branchName);
}

// get contributors (from contributors map)
export function getContributors(data: RepositoryData): string[] {
  return Array.from(data.contributors.values()).map(contributor => contributor.name);
}

// get users (from commits)
export function getUsers(data: RepositoryData): string[] {
  const userSet = new Set<string>();
  data.allCommits.forEach(commit => userSet.add(commit.contributorName));
  return Array.from(userSet);
}

// 4️⃣ get LOC line data (Lines of codes changed over time)
export function getLocLineData(data: RepositoryData): {
  title: string;
  data: { [key: string]: number | string }[];
} {
  const dateMap = new Map<string, { [user: string]: number }>();

  data.allCommits.forEach(commit => {
    const user = commit.contributorName;
    const loc = getLOCFromCommit(commit);
    const dateStr = commit.timestamp.toISOString().split("T")[0];

    if (!dateMap.has(dateStr)) dateMap.set(dateStr, {});
    dateMap.get(dateStr)![user] = (dateMap.get(dateStr)![user] || 0) + loc;
  });

  const sorted = Array.from(dateMap.entries())
    .sort(([a], [b]) => a.localeCompare(b))
    .map(([date, userData]) => ({ date, ...userData }));

  return {
    title: "Lines of Codes Changed Over Time",
    data: sorted
  };
}

// 5️⃣ get total commits per contributor
export function getAllContributorsCommits(data: RepositoryData): {
  title: string;
  data: { name: string; commits: number }[];
} {
  const commitMap = new Map<string, number>();

  data.allCommits.forEach(commit => {
    const user = commit.contributorName;
    commitMap.set(user, (commitMap.get(user) || 0) + 1);
  });

  const result = Array.from(commitMap.entries()).map(([name, commits]) => ({ name, commits }));

  return {
    title: "All Contributor Commits",
    data: result
  };
}

// get total LOC per user
export function getTotalLocData(data: RepositoryData): { name: string; value: number }[] {
  const locMap = new Map<string, number>();

  data.allCommits.forEach(commit => {
    const user = commit.contributorName;
    const loc = getLOCFromCommit(commit);
    locMap.set(user, (locMap.get(user) || 0) + loc);
  });

  return Array.from(locMap.entries()).map(([name, value]) => ({ name, value }));
}
