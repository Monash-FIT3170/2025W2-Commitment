import { Meteor } from "meteor/meteor";
import { RepositoryData, CommitData } from "/server/commitment_api/types";
import { Subject } from "rxjs";


/**
 * Return a list of all branches
 */

export const getBranches = (repo: RepositoryData): string[] =>
  // get list of branch names
  repo.branches.map((branch) => branch.branchName);

/**
 * Return a list of all contributors
 */
export const getContributors = (repo: RepositoryData): string[] => {
  const contributorsMap = repo.contributors;
  return Array.from(contributorsMap.values()).map(
    (contributor) => contributor.name
  );
};

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

  data.allCommits.forEach((commit) => {
    const user = commit.contributorName;
    counts.set(user, (counts.get(user) ?? 0) + 1);
  });

  const list = Array.from(counts.entries()).map(([name, commits]) => ({
    name,
    commits,
  }));
  console.log("All contributor commits:", list);
  return { title: "All Contributor Commits", data: list };
}

/**
 *
 * @param repo
 * @returns
 */
export const calculateTotalCommits = (
  repo: RepositoryData
): {
  total: number;
  percentageChange: number;
  isPositive: boolean;
  data: { value: number }[];
} => {
  const total = repo.allCommits.size;

  // Group commits by day
  const commitsByDay = new Map<string, number>();
  repo.allCommits.forEach((commit) => {
    const day = new Date(commit.timestamp).toISOString().slice(0, 10);
    commitsByDay.set(day, (commitsByDay.get(day) ?? 0) + 1);
  });

  // Sort days chronologically
  const sortedDays = Array.from(commitsByDay.entries()).sort(
    (a, b) => new Date(a[0]).getTime() - new Date(b[0]).getTime()
  );

  // Prepare graph data (daily counts)
  const data = sortedDays.map(([_, count]) => ({ value: count }));

  // Calculate percentage change = today vs yesterday
  let percentageChange = 0;
  let isPositive = true;
  if (sortedDays.length >= 2) {
    const last = sortedDays[sortedDays.length - 1][1];
    const prev = sortedDays[sortedDays.length - 2][1];
    percentageChange = prev === 0 ? 100 : ((last - prev) / prev) * 100;
    isPositive = last >= prev;
  }

  return {
    total,
    percentageChange,
    isPositive,
    data,
  };
};
