import { HeatMapData, FilteredData } from "/imports/api/types";

/**
 * HEATMAP FUNCTIONS
 */

export function heatMapLOC(data: FilteredData): HeatMapData[] {
  const repoData = data.repositoryData;
  // Map of date → user → LOC count
  const byDateUser = new Map<string, Record<string, number>>();

  repoData.allCommits.forEach((commit) => {
    const user = commit.value.contributorName;
    const date = new Date(commit.value.timestamp).toISOString().split("T")[0];

    // calculate LOC for this commit
    const locthisCommit = commit.value.fileData.reduce(
      (sum, fileChange) => sum + fileChange.file.contents.split("\n").length,
      0
    );

    if (!byDateUser.has(date)) byDateUser.set(date, {});
    const bucket = byDateUser.get(date)!;
    bucket[user] = (bucket[user] ?? 0) + locthisCommit;
  });

  // convert to heatmapdata[]
  const heatMapArray: HeatMapData[] = [];

  byDateUser.forEach((userCounts, date) => {
    Object.entries(userCounts).forEach(([user, count]) => {
      heatMapArray.push({
        name: user,
        date,
        count,
      });
    });
  });

  return heatMapArray;
}

export function heatMapLOCPerCommit(data: FilteredData): HeatMapData[] {
  const repoData = data.repositoryData;

  // Map of date → user → { totalLOC, commitCount }
  const byDateUser = new Map<
    string,
    Record<string, { totalLOC: number; commitCount: number }>
  >();

  repoData.allCommits.forEach((commit) => {
    const user = commit.value.contributorName;
    const date = new Date(commit.value.timestamp).toISOString().split("T")[0];

    // calculate LOC for this commit
    const locThisCommit = commit.value.fileData.reduce(
      (sum, fileChange) => sum + fileChange.file.contents.split("\n").length,
      0
    );

    if (!byDateUser.has(date)) byDateUser.set(date, {});
    const bucket = byDateUser.get(date)!;

    if (!bucket[user]) bucket[user] = { totalLOC: 0, commitCount: 0 };

    bucket[user].totalLOC += locThisCommit;
    bucket[user].commitCount += 1;
  });

  // Convert to HeatMapData[] with LOC per commit
  const heatMapArray: HeatMapData[] = [];
  byDateUser.forEach((userCounts, date) => {
    for (const [user, { totalLOC, commitCount }] of Object.entries(
      userCounts
    )) {
      heatMapArray.push({
        name: user,
        date,
        count: commitCount > 0 ? totalLOC / commitCount : 0, // LOC per commit
      });
    }
  });

  return heatMapArray;
}

/**
 * Calculates the average number of commits per day for each contributor.
 * @param data The filtered repository data
 * @returns A heat map data structure containing name, date and count for each contributors average number of commits per day.
 */
export function heatMapCommitsPerDay(data: FilteredData): HeatMapData[] {
  const repoData = data.repositoryData;
  const { start, end } = data.dateRange;

  // Calculate number of days in range (inclusive)
  const daysInRange =
    Math.floor((end.getTime() - start.getTime()) / (1000 * 60 * 60 * 24)) + 1;

  // Map of date → user → commit count
  const byDateUser = new Map<string, Record<string, number>>();

  repoData.allCommits.forEach((commit) => {
    const user = commit.value.contributorName;
    const date = new Date(commit.value.timestamp).toISOString().split("T")[0];

    if (!byDateUser.has(date)) byDateUser.set(date, {});
    const bucket = byDateUser.get(date)!;

    bucket[user] = (bucket[user] ?? 0) + 1;
  });

  // Flatten into HeatMapData[] with average per day
  const heatMapArray: HeatMapData[] = [];

  byDateUser.forEach((userCounts, date) => {
    Object.entries(userCounts).forEach(([user, count]) => {
      const avgCount = count / daysInRange; // normalize per day
      heatMapArray.push({
        name: user,
        date,
        count: avgCount,
      });
    });
  });

  return heatMapArray;
}

export function heatMapTotalCommits(data: FilteredData): HeatMapData[] {
  const repoData = data.repositoryData;

  // Map of date → user → commit count
  const byDateUser = new Map<string, Record<string, number>>();

  repoData.allCommits.forEach((commit) => {
    const user = commit.value.contributorName;
    const date = new Date(commit.value.timestamp).toISOString().split("T")[0];

    if (!byDateUser.has(date)) byDateUser.set(date, {});
    const bucket = byDateUser.get(date)!;

    bucket[user] = (bucket[user] ?? 0) + 1;
  });

  // Flatten into HeatMapData[]
  const heatMapArray: HeatMapData[] = [];

  byDateUser.forEach((userCounts, date) => {
    Object.entries(userCounts).forEach(([user, count]) => {
      heatMapArray.push({
        name: user,
        date,
        count,
      });
    });
  });

  return heatMapArray;
}
