// // server/metrics_transformers.ts

// import { TimeLOCEntry, UserStats, ScalingData, getTotalCommitsAndLOCPerUser } from "./metrics";
// import { RepositoryData } from "./commitment_api/types";

// // LOC Over Time -> Graph-compatible format
// export function transformLOCMapToDataset(locMap: Map<string, TimeLOCEntry[]>): {
//   title: string;
//   data: { [key: string]: number | string }[];
// } {
//   const dateMap = new Map<string, { [user: string]: number }>();

//   for (const [user, entries] of locMap.entries()) {
//     for (const entry of entries) {
//       const dateStr = entry.timestamp.toISOString().split("T")[0];
//       if (!dateMap.has(dateStr)) dateMap.set(dateStr, {});
//       dateMap.get(dateStr)![user] = (dateMap.get(dateStr)![user] || 0) + entry.locChanged;
//     }
//   }

//   const data = Array.from(dateMap.entries())
//     .sort(([a], [b]) => a.localeCompare(b))
//     .map(([date, userData]) => ({ date, ...userData }));

//   return {
//     title: "Lines of Codes Changed Over Time",
//     data
//   };
// }

// // Total commits -> graph-compatible list
// export function transformCommitStatsToList(stats: Map<string, UserStats>): {
//   name: string; commits: number;
// }[] {
//   return Array.from(stats.entries()).map(([name, value]) => ({
//     name,
//     commits: value.commits
//   }));
// }

// // Aggregate scaling -> graph-compatible list
// export function transformScalingMapToList(scaling: Map<string, ScalingData>): {
//   name: string; score: number;
// }[] {
//   return Array.from(scaling.entries()).map(([name, value]) => ({
//     name,
//     score: value.score
//   }));
// }

// // NEW: Repo overview data extractor (branches, users, contributors, total LOC)
// export function extractRepoOverviewData(data: RepositoryData): {
//   branches: string[];
//   contributors: string[];
//   users: string[];
//   totalLoc: { name: string; value: number }[];
// } {
//   // Branch names from repository
//   const branches = data.branches.map(branch => branch.branchName);

//   // Contributor names from contributor map
//   const contributors = Array.from(data.contributors.values()).map(c => c.name);

//   // Unique users from allCommits
//   const userSet = new Set<string>();
//   data.allCommits.forEach(commit => userSet.add(commit.contributorName));
//   const users = Array.from(userSet);

//   // Total LOC per user using existing metric
//   const locMap = getTotalCommitsAndLOCPerUser(data);
//   const totalLoc = Array.from(locMap.entries()).map(([name, stats]) => ({
//     name,
//     value: stats.loc
//   }));

//   return {
//     branches,
//     contributors,
//     users,
//     totalLoc
//   };
// }
