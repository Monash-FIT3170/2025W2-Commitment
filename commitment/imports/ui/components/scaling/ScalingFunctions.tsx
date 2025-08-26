import { RepositoryData, CommitData } from "/imports/api/types";

// All possible metrics (same as Python)
const USER_DATA_METRICS = [
  "Total No. Commits",
  "LOC",
  "LOC per commit",
  "Commits per day",
];

function normalizeMetric(values: number[], alpha = 0.1): number[] {
  const minVal = Math.min(...values);
  const maxVal = Math.max(...values);
  const rangeVal = maxVal !== minVal ? maxVal - minVal : 1;

  const normalized = values.map((v) => (v - minVal) / rangeVal);
  return normalized.map((n) => alpha + (1 - 2 * alpha) * n);
}

export function scaleUsers(
  repoData: RepositoryData,
  config: { metrics: string[]; method: string }
) {
  const selectedMetrics = config.metrics ?? [];
  const method = config.method ?? "Percentiles";

  // Build users array with all metric values
  const userNames = Array.from(
    new Set(
      Array.from(repoData.allCommits.values()).map((c) => c.contributorName)
    )
  );

  const users: [string, number[]][] = userNames.map((name) => {
    const commits = Array.from(repoData.allCommits.values()).filter(
      (c) => c.contributorName === name
    );

    const values = USER_DATA_METRICS.map((metric) => {
      switch (metric) {
        case "Total No. Commits":
          return commits.length;
        case "LOC":
          return commits.reduce((sum, c) => sum + getLOCFromCommit(c), 0);
        case "LOC per commit":
          return commits.length
            ? commits.reduce((sum, c) => sum + getLOCFromCommit(c), 0) /
                commits.length
            : 0;
        case "Commits per day":
          if (commits.length === 0) return 0;
          const timestamps = commits.map((c) => c.timestamp.getTime());
          const days =
            (Math.max(...timestamps) - Math.min(...timestamps)) /
              (1000 * 60 * 60 * 24) || 1;
          return commits.length / days;
        default:
          return 0;
      }
    });

    return [name, values];
  });

  const metricIndices = selectedMetrics
    .map((m) => USER_DATA_METRICS.indexOf(m))
    .filter((idx) => idx >= 0);

  if (metricIndices.length === 0) {
    return users.map(([name]) => ({ name, score: 0.0 }));
  }

  const metricsValues: number[][] = metricIndices.map((idx) =>
    normalizeMetric(users.map((u) => u[1][idx]))
  );

  return users.map(([name], i) => {
    const userScores = metricsValues.map((mv) => mv[i]);
    let score: number;

    if (method === "Percentiles") {
      score = userScores.reduce((a, b) => a + b, 0) / userScores.length;
    } else if (method === "Mean +/- Std") {
      const mean = userScores.reduce((a, b) => a + b, 0) / userScores.length;
      const std = Math.sqrt(
        userScores.reduce((sum, x) => sum + (x - mean) ** 2, 0) /
          userScores.length
      );
      score = Math.min(Math.max(mean + std, 0.0), 1.0);
    } else if (method === "Quartiles") {
      const sorted = [...userScores].sort((a, b) => a - b);
      const mid = Math.floor(sorted.length / 2);
      score =
        sorted.length % 2 === 0
          ? (sorted[mid - 1] + sorted[mid]) / 2
          : sorted[mid];
    } else {
      score = userScores.reduce((a, b) => a + b, 0) / userScores.length;
    }

    return { name, score: Math.round(score * 100) / 100 };
  });
}

// Helper to calculate LOC from a commit
function getLOCFromCommit(commit: CommitData): number {
  if (!commit.fileData) return 0;
  return commit.fileData.reduce((acc, fileChange) => {
    const content = fileChange.file?.contents || "";
    return acc + content.split("\n").length;
  }, 0);
}
