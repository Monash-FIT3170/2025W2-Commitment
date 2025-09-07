import { Meteor } from "meteor/meteor"

import { ScalingConfig } from "../imports/ui/components/scaling/ScalingConfigForm";
import {
  UserScalingSummary,
  AllMetricsData,
  SerializableRepoData,
  ContributorValueWithAliases,
} from "../imports/api/types";
import { applyAliasMapping, applyAliasMappingIfNeeded, createAliasMapping, getUserAliasConfig } from "./alias_mapping";

// All the Metrics being considered
const DEFAULT_METRICS = [
  "Total No. Commits",
  "LOC",
  "LOC Per Commit",
  "Commits Per Day",
];

// The order these functions work in are as such:
// getScaledResults() calls scaleUsers() which will buildUsers() and normaliseMetric() to get name, emails, and associated scales



/**
 * this method takes in raw metric data and applies an alpha value to it. In essence, it scales an array of numbers to a bounded 
 range [alpha, 1-alpha], mapping the smallest value to alpha, the largest to 1-alpha, and proportionally adjusting all intermediate values. 

 * @param values the metric data
 * @param alpha controls the minimum and maximum range
 * @returns the normalised set of values
 */
function normaliseMetric(values: Array<number | null>, alpha = 0.2): number[] {
  const present = values.filter(
    (v): v is number => v !== null && Number.isFinite(v)
  );
  if (!present.length) return values.map(() => 0.5); // a fallback to 0.5 if for some reason the metric cannot be found

  const min = Math.min(...present);
  const max = Math.max(...present);
  const range = max !== min ? max - min : 1;

  return values.map((v) =>
    v !== null && Number.isFinite(v)
      ? alpha + ((v - min) / range) * (1 - 2 * alpha)
      : alpha
  );
}


/**
 * Builds an array of user data objects containing selected metric values.
 *
 * For each user in 'allMetrics', it extracts the values of 'selectedMetrics',
 * converting invalid or non-numeric values to 'null'.
 *
 * @param allMetrics - An object mapping user names to their metrics.
 * @param selectedMetrics - An array of metric names to extract for each user.
 * @returns An array of objects, each with:
 *   - name: the user’s name
 *   - values: an array of numbers or null corresponding to the selected metrics
 */

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

/**
 * Computes the percentile rank of a value within an array of numbers.
 *
 * The percentile rank is the position of value in the sorted array,
 * scaled to the range [0, 1]. If the value is not found, returns 0.5 as a neutral fallback.
 *
 * @param values - Array of numbers to rank within.
 * @param value - The number whose percentile rank is computed.
 * @returns A number between 0 and 1 representing the percentile rank.
 */
function percentileRank(values: number[], value: number): number {
  const sorted = [...values].sort((a, b) => a - b);
  const index = sorted.findIndex((v) => v === value);

  if (index === -1) return 0.5; // neutral if missing
  return index / (sorted.length - 1 || 1); // scale to [0,1]
}


function smallGroupPercentileRank(values: number[], value: number): number {
  // Apply small-group smoothing logic from smallGroupPercentile
  const sorted = [...values].sort((a, b) => a - b);
  const idx = sorted.indexOf(value);
  if (idx === -1) return 0.5;
  return (idx + 1) / (values.length + 1); // use values.length instead of sorted.length
}

/**
 * Computes scaled scales for users based on selected metrics from a repository.
 *
 * Steps:
 * 1. Fetches all metrics for the given repository via a Meteor call.
 * 2. Builds user data objects containing only the selected metrics.
 * 3. Normalizes each metric column using `normaliseMetric`.
 * 4. Computes a score per user based on the specified method:
 *    - "Percentiles": averages percentile ranks of each metric.
 *    - "Mean +/- Std": mean plus standard deviation of the normalized metrics.
 *    - "Quartiles": median of the normalized metrics.
 *    - Default: simple mean of normalized metrics.
 *
 * @param repoUrl - The repository URL to fetch metrics from.
 * @param config - Configuration object specifying metrics to use and the scoring method.
 * @returns A promise resolving to an array of objects with:
 *   - name: the user’s name
 *   - score: the user’s scaled score rounded to two decimal places.
 */
async function scaleUsers(repoUrl: string, config: ScalingConfig) {
  const allMetrics = await Meteor.callAsync("repo.getAllMetrics", { repoUrl });

  const selectedMetrics = config.metrics?.length
    ? config.metrics
    : DEFAULT_METRICS;

  let method = config.method ?? "Percentiles";

  const users = buildUsers(allMetrics, selectedMetrics);
  if (!users.length) return [];

  let metricsValues: number[][];

  if (users.length <= 3) {
    console.warn("Small group detected (<=3 users). Using raw values for percentile ranking.");
    metricsValues = selectedMetrics.map((_, i) => {
    const colValues = users
      .map(u => u.values[i])
      .filter((v): v is number => v !== null && Number.isFinite(v));
    
    return users.map(u => {
      const v = u.values[i];
      if (v === null || !Number.isFinite(v)) return 0.5;
      return smallGroupPercentileRank(colValues, v); // map to 0-1 scale
    });
  });

  } else {
    metricsValues = selectedMetrics.map((_, i) =>
      normaliseMetric(users.map((u) => u.values[i]))
    );
  }


  return users.map((user, idx) => {
    const scales = metricsValues.map((col) => col[idx]);
    let score: number;

    switch (method) {
      case "Percentiles": {
        const percentileScores = selectedMetrics.map((_, colIdx) => {
          const colValues: number[] = users
            .map((u) => u.values[colIdx])
            .filter((v): v is number => v !== null && Number.isFinite(v));

          return users.map((u) => {
            const v = u.values[colIdx];
            if (v === null || !Number.isFinite(v)) return 0.5;
            // Use small-group-safe percentile
            return users.length <= 3
              ? smallGroupPercentileRank(colValues, v)
              : percentileRank(colValues, v);
          });
        });

        score =
          percentileScores.reduce((sum, col) => sum + col[idx], 0) /
          selectedMetrics.length;
        break;
      }

      case "Mean +/- Std":
        const mean = scales.reduce((a, b) => a + b, 0) / scales.length;
        const std = Math.sqrt(
          scales.reduce((sum, x) => sum + (x - mean) ** 2, 0) / scales.length
        );
        score = mean + std;
        break;

      case "Quartiles":
        const sorted = [...scales].sort((a, b) => a - b);
        const mid = Math.floor(sorted.length / 2);
        score =
          sorted.length % 2 === 0
            ? (sorted[mid - 1] + sorted[mid]) / 2
            : sorted[mid];
        break;

      default: // Default (simple mean)
        score = scales.reduce((a, b) => a + b, 0) / scales.length;
        break;
    }

    return { name: user.name, score: Math.round(score * 100) / 100 };
  });
}


/**
 * Generates a detailed scaled results summary for each contributor in a repository.
 *
 * 1. Uses 'scaleUsers' to compute normalized scales for all users based on the provided config.
 * 2. Matches each user to their contributor data from 'repoData'.
 * 3. Constructs a list of aliases for each contributor based on their emails.
 * 4. Returns an array of user summaries including name, aliases, finalGrade (initially null), and scaled score.
 *
 * @param repoData - Serializable repository data containing contributor information.
 * @param config - Configuration specifying which metrics to scale and the scaling method.
 * @param repoUrl - URL of the repository to fetch metrics from.
 * @returns A promise resolving to an array of 'UserScalingSummary' objects with:
 *   - name: contributor's name
 *   - aliases: list of objects mapping username to email
 *   - finalGrade: initially null
 *   - scale: the computed scaled score
 */


export async function getScaledResults(
  repoData: SerializableRepoData,
  config: ScalingConfig,
  repoUrl: string,
  userId: string
): Promise<UserScalingSummary[]> {

  const aliasConfig = await Meteor.callAsync("aliasConfigs.getAllForOwner", userId).catch(() => null);

  const mappedData =
  aliasConfig && aliasConfig.length
    ? {
        ...applyAliasMapping(repoData, createAliasMapping(aliasConfig[0].aliases)),
        contributors: applyAliasMapping(repoData, createAliasMapping(aliasConfig[0].aliases)).contributors.map(c => ({
          ...c,
          value: {
            ...c.value,
            aliases: [], 
          } as ContributorValueWithAliases
        }))
      }
    : {
        ...repoData,
        contributors: repoData.contributors.map(c => ({
          ...c,
          value: {
            ...c.value,
            aliases: [],
          } as ContributorValueWithAliases
        }))
      };

  const updatedContributors = await Promise.all(
    mappedData.contributors.map(async (contributor) => {
      const { key, value } = contributor;

      const extraAliases = await Meteor.callAsync("aliasConfigs.getAliasesFor", key).catch(() => ({
        gitUsernames: [],
        emails: [],
      }));

      // Combine all unique emails to avoid duplicates
      const allEmails = [...new Set([...value.emails, ...(extraAliases.emails || [])])];
      
      const aliases = [
        ...allEmails.map((email) => ({ username: key, email })),
        ...(extraAliases.gitUsernames || [])
          .filter((u:string) => u !== key)
          .map((username:string) => ({ username, email: null })),
      ];

      const valWithAliases: ContributorValueWithAliases = {
        ...value,
        emails: [...new Set([...value.emails, ...(extraAliases.emails || [])])],
        aliases,
      };

      return {
        ...contributor,
        value: valWithAliases,
      };
    })
  );

  const mappedDataWithAliases: SerializableRepoData = {
    ...mappedData,
    contributors: updatedContributors,
  };

  const validUserNames = new Set(
    mappedDataWithAliases.contributors
      .filter((c) => c.value.emails.length > 0)
      .map((c) => c.key)
  );

  const scaledUsers = await scaleUsers(repoUrl, config);
  const scaledValidUsers = scaledUsers.filter(({ name }) => validUserNames.has(name));

  const finalResults: UserScalingSummary[] = mappedDataWithAliases.contributors.map(c => {
  const contributorValue = c.value as ContributorValueWithAliases;

  // Use only the canonical name for scale lookup
  const scaledUser = scaledUsers.find(u => u.name === c.key);

  return {
    name: c.key,
    aliases: contributorValue.aliases,
    finalGrade: null,
    scale: scaledUser ? scaledUser.score : 0,
  };
});

  return finalResults;
}
