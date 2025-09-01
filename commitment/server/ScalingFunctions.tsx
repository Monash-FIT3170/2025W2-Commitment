import { ScalingConfig } from "/imports/ui/components/scaling/ScalingConfigForm";
import {
  RepositoryData,
  UserScalingSummary,
  AllMetricsData,
  SerialisableMapObject,
  ContributorData,
  SerializableRepoData,
} from "/imports/api/types";
import { getContributors } from "./repo_metrics";
import {
  deserializeRepoData,
  serializeRepoData,
} from "/imports/api/serialisation";

// Default metrics if none selected
const DEFAULT_METRICS = [
  "Total No. Commits",
  "LOC",
  "LOC per commit",
  "Commits per day",
];

// Normalize metric values to [alpha, 1-alpha] range
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

// Helper: sanitize and resolve metric names
function normalizeKey(key: string): string {
  return key.toLowerCase().replace(/\s|\/|_/g, "");
}

function buildUsers(
  allMetrics: AllMetricsData,
  selectedMetrics: string[]
): { name: string; values: (number | null)[] }[] {
  const wantedKeys = selectedMetrics.map(normalizeKey);

  return Object.entries(allMetrics).map(([name, metrics]) => {
    // build values by iterating wantedKeys, not by direct indexing
    const values = wantedKeys.map((wk) => {
      // find a key in metrics that normalizes to wk
      const match = Object.keys(metrics).find((k) => normalizeKey(k) === wk);
      const val = match ? metrics[match as keyof typeof metrics] : null;
      return Number.isFinite(val as number) ? (val as number) : null;
    });

    return { name, values };
  });
}

// Scale users based on selected metrics and method
async function scaleUsers(repoUrl: string, config: ScalingConfig) {
  const allMetrics = await Meteor.callAsync("repo.getAllMetrics", {
    repoUrl
  });

  console.log("All metrics at scaleUsers:", allMetrics);

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
      case "Percentiles":
        score = scores.reduce((a, b) => a + b, 0) / scores.length;
        break;

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
  console.log("RepoURl: ", repoUrl); 
  const scaledUsers = await scaleUsers(repoUrl, config);

  // contributors is an array of { key, value: { name, emails } }
  const contributors = repoData.contributors; 

    // serializeRepoData(repoData).contributors;
  console.log("here: ", contributors);
  return scaledUsers.map(({ name, score }) => {
    // find contributor by matching value.name
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
