import { ScalingConfig } from "/imports/ui/components/scaling/ScalingConfigForm";
import {
  UserScalingSummary,
  AllMetricsData,
  SerializableRepoData,
} from "/imports/api/types";

// All the Metrics being considered
const DEFAULT_METRICS = [
  "Total No. Commits",
  "LOC",
  "LOC Per Commit",
  "Commits Per Day",
];

// Scaling Functions



/**
 * 
 * @param values 
 * @param alpha 
 * @returns 
 */
function normalizeMetric(values: Array<number | null>, alpha = 0.2): number[] {
  const present = values.filter(
    (v): v is number => v !== null && Number.isFinite(v)
  );
  if (!present.length) return values.map(() => 0.5); // all missing → neutral

  const min = Math.min(...present);
  const max = Math.max(...present);
  const range = max !== min ? max - min : 1;

  return values.map((v) =>
    v !== null && Number.isFinite(v)
      ? alpha + ((v - min) / range) * (1 - 2 * alpha)
      : alpha
  );
}

function buildUsers(
  allMetrics: AllMetricsData,
  selectedMetrics: string[]
): { name: string; values: (number | null)[] }[] {
  return Object.entries(allMetrics).map(([name, metrics]) => {
    const values = selectedMetrics.map((metricName) => {
      const val = metrics[metricName as keyof typeof metrics];
      return Number.isFinite(val as number) ? (val as number) : null;
    });

    return { name, values };
  });
}

// Compute percentile rank of a value within an array
function percentileRank(values: number[], value: number): number {
  const sorted = [...values].sort((a, b) => a - b);
  const index = sorted.findIndex((v) => v === value);

  if (index === -1) return 0.5; // neutral if missing
  return index / (sorted.length - 1 || 1); // scale to [0,1]
}

// Scale users based on selected metrics and method
async function scaleUsers(repoUrl: string, config: ScalingConfig) {
  const allMetrics = await Meteor.callAsync("repo.getAllMetrics", { repoUrl });

  const selectedMetrics = config.metrics?.length
    ? config.metrics
    : DEFAULT_METRICS;
  const method = config.method ?? "Percentiles";

  // Use helper to build users
  const users = buildUsers(allMetrics, selectedMetrics);

  if (!users.length) return [];

  // Normalize metrics column-wise
  const metricsValues = selectedMetrics.map((_, i) =>
    normalizeMetric(users.map((u) => u.values[i]))
  );

  // Calculate score per user
  return users.map((user, idx) => {
    const scores = metricsValues.map((col) => col[idx]);

    let score: number;
    switch (method) {
      case "Percentiles": {
        // compute percentile rank per column
        const percentileScores = selectedMetrics.map((_, colIdx) => {
          // filter out nulls for ranking
          const colValues: number[] = users
            .map((u) => u.values[colIdx])
            .filter((v): v is number => v !== null && Number.isFinite(v));

          return users.map((u) => {
            const v = u.values[colIdx];
            if (v === null || !Number.isFinite(v)) return 0.5; // neutral for missing
            return percentileRank(colValues, v);
          });
        });

        score =
          percentileScores.reduce((sum, col) => sum + col[idx], 0) /
          selectedMetrics.length;
        break;
      }

      case "Mean +/- Std":
        const mean = scores.reduce((a, b) => a + b, 0) / scores.length;
        const std = Math.sqrt(
          scores.reduce((sum, x) => sum + (x - mean) ** 2, 0) / scores.length
        );
        score = mean + std;
        break;

      case "Quartiles":
        const sorted = [...scores].sort((a, b) => a - b);
        const mid = Math.floor(sorted.length / 2);
        score =
          sorted.length % 2 === 0
            ? (sorted[mid - 1] + sorted[mid]) / 2
            : sorted[mid];
        break;

      default:
        score = scores.reduce((a, b) => a + b, 0) / scores.length;
        break;
    }

    return { name: user.name, score: Math.round(score * 100) / 100 };
  });
}

export async function getScaledResults(
  repoData: SerializableRepoData,
  config: ScalingConfig,
  repoUrl: string
): Promise<UserScalingSummary[]> {
  const scaledUsers = await scaleUsers(repoUrl, config);

  const contributors = repoData.contributors;

  return scaledUsers.map(({ name, score }) => {
    const contributor = contributors.find((c) => c.value.name === name);

    const aliases = contributor
      ? contributor.value.emails.map((email) => ({ username: name, email }))
      : [];

    return {
      name,
      aliases,
      finalGrade: null,
      scale: score,
    };
  });
}
