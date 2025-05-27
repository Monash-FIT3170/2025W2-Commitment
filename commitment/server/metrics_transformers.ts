// server/metrics_transformers.ts

import { TimeLOCEntry, UserStats, ScalingData } from "./metrics";

// LOC Over Time -> Graph-compatible format

export function transformLOCMapToDataset(locMap: Map<string, TimeLOCEntry[]>): {
  title: string;
  data: { [key: string]: number | string }[];
} {
  const dateMap = new Map<string, { [user: string]: number }>();

  for (const [user, entries] of locMap.entries()) {
    for (const entry of entries) {
      const dateStr = entry.timestamp.toISOString().split("T")[0];
      if (!dateMap.has(dateStr)) dateMap.set(dateStr, {});
      dateMap.get(dateStr)![user] = (dateMap.get(dateStr)![user] || 0) + entry.locChanged;
    }
  }

  const data = Array.from(dateMap.entries())
    .sort(([a], [b]) => a.localeCompare(b))
    .map(([date, userData]) => ({ date, ...userData }));

  return {
    title: "Lines of Codes Changed Over Time",
    data
  };
}

//Total commits -> graph-Compatible list

export function transformCommitStatsToList(stats: Map<string, UserStats>): {
  name: string; commits: number;
}[] {
  return Array.from(stats.entries()).map(([name, value]) => ({
    name,
    commits: value.commits
  }));
}

// Aggregate scaling -> Graph-Compatible list 

export function transformScalingMapToList(scaling: Map<string, ScalingData>): {
  name: string; score: number;
}[] {
  return Array.from(scaling.entries()).map(([name, value]) => ({
    name,
    score: value.score
  }));
}