import { Meteor } from "meteor/meteor";
import { ScalingConfig } from "../imports/ui/components/scaling/ScalingConfigForm";
import {
  UserScalingSummary,
  AllMetricsData,
  SerializableRepoData,
  ContributorValueWithAliases,
  ScoreFn,
  SerialisableMapObject,
} from "../imports/api/types";
import {
  applyAliasMapping,
  createAliasMapping,
} from "./alias_mapping";
import { StudentAlias } from "/imports/api/alias_configs";
import { composeVisitors } from "lightningcss";


/**
GENERAL FLOW OF SCALING:

1. Fetch alias configuration for the repository owner
   - determines contributor names and aliases
2. Apply alias mapping to repo contributors
3. Enrich contributors with combined emails and git usernames
4. Fetch all repository metrics (per contributor)
5. Build structured user metric objects
    - only include selected metrics, convert invalid values to null
6. Normalize metrics (or apply small-group percentile ranking if ≤ 3 users)
    - consider creating a separate method for this in Milestone 4
7. Score users based on the selected method (Percentiles, Mean +/- Std, Quartiles, Compact Scaling, Default)
8. Combine scores with contributor metadata
9. Return final array of UserScalingSummary
*/



const DEFAULT_METRICS = ["Total No. Commits", "LOC", "LOC Per Commit", "Commits Per Day", "Compact Scaling"];

//
// ---------- Normalization helpers! ----------
//

export function normaliseMetric(values: Array<number | null>, alpha = 0.2): number[] {
  const present = values.filter((v): v is number => v !== null && Number.isFinite(v));
  if (!present.length) return values.map(() => 0.5);

  const min = Math.min(...present);
  const max = Math.max(...present);
  const range = max !== min ? max - min : 1;

  return values.map((v) =>
    v !== null && Number.isFinite(v) ? alpha + ((v - min) / range) * (1 - 2 * alpha) : alpha
  );
}

function percentileRank(values: number[], value: number): number {
  const sorted = [...values].sort((a, b) => a - b);
  const index = sorted.findIndex((v) => v === value);
  return index === -1 ? 0.5 : index / (sorted.length - 1 || 1);
}

function smallGroupPercentileRank(values: number[], value: number): number {
  const sorted = [...values].sort((a, b) => a - b);
  const idx = sorted.indexOf(value);
  return idx === -1 ? 0.5 : (idx + 1) / (values.length + 1);
}

//
// ---------- User building ----------
//

function buildUsers(
  allMetrics: AllMetricsData,
  selectedMetrics: string[]
): { name: string; values: (number | null)[] }[] {

  return Object.entries(allMetrics).map(([name, metrics]) => ({
    name,
    values: selectedMetrics.map((metricName) => {
      const val = metrics[metricName as keyof typeof metrics];
      return Number.isFinite(val) ? val : null;
    }),
  }));
}

//
// ---------- Scoring strategies ----------
//

const scoringStrategies: Record<string, ScoreFn> = {
  Percentiles: (_, idx, users, selectedMetrics) => {
        const percentileScores = selectedMetrics.map((_metric, colIdx) => {
            const colValues = users
                .map((u) => u.values[colIdx])
                .filter((v): v is number => v !== null);

            return users.map((u) => {
                const val = u.values[colIdx];
                if (val == null) return 0.5;

                if (users.length <= 3) {
                    return smallGroupPercentileRank(colValues, val);
                }
                
                return percentileRank(colValues, val);
            });
        });

    return percentileScores.reduce((sum, col) => sum + col[idx], 0) / selectedMetrics.length;
  },

  "Mean +/- Std": (scales) => {
  const mean = scales.reduce((a, b) => a + b, 0) / scales.length;
  const std = Math.sqrt(scales.reduce((sum, x) => sum + (x - mean) ** 2, 0) / scales.length);
  const max = Math.max(...scales);
  const min = Math.min(...scales);
  const cappedStd = Math.min(std, (max - min) / 2); // cap at half range
  return mean + cappedStd;
},

  Quartiles: (scales) => {
    const sorted = [...scales].sort((a, b) => a - b);
    const mid = Math.floor(sorted.length / 2);
    return sorted.length % 2 === 0 ? (sorted[mid - 1] + sorted[mid]) / 2 : sorted[mid];
  },

"Compact Scaling": (scales: number[]) => {
    if (!scales.length) return 1;

    const minScale = 0;
    const maxScale = 1.2;

    // Compute mean and std
    const mean = scales.reduce((a, b) => a + b, 0) / scales.length;
    const variance = scales.reduce((sum, x) => sum + (x - mean) ** 2, 0) / scales.length;
    const std = Math.sqrt(variance);

    // Handle tiny variance or single value
    if (std < 1e-6) {
        // Map single or identical values proportionally to [0.5, 1.2] baseline
        const value = scales[0];
        return 0.5 + value * 0.7; // ensures floor at 0.5, max at 1.2
    }

    // Smooth scaling using tanh
    const smoothScale = (x: number) => {
        const normalized = (x - mean) / std;
        const scale = 0.5 + Math.tanh(normalized * 0.8) * 0.5; // between 0 and 1
        return 0.5 + scale * 0.7; // floor 0.5, max ~1.2
    };

    const scaledValues = scales.map(smoothScale);
    const avgScaled = scaledValues.reduce((a, b) => a + b, 0) / scaledValues.length;

    return Math.min(avgScaled, maxScale);
},



  Default: (scales) => scales.reduce((a, b) => a + b, 0) / scales.length, // just calculates the mean (average) of the user’s normalized metric values
};

//
// ---------- Scaling ----------
//

async function scaleUsers(repoUrl: string, config: ScalingConfig) {

  const allMetrics = await Meteor.callAsync("repo.getAllMetrics", { repoUrl }) as AllMetricsData; // IS CASTING LIKE THIS OKAY?

  const selectedMetrics = config.metrics?.length ? config.metrics : DEFAULT_METRICS;
  const method = config.method ?? "Percentiles";

  const users = buildUsers(allMetrics, selectedMetrics);
  if (!users.length) return [];

  const metricsValues =
  users.length <= 3
    ? selectedMetrics.map((_, i) => {
        const colValues = users
          .map((u) => u.values[i])
          .filter((v): v is number => v !== null);

        return users.map((u) => {
          const val = u.values[i];
          if (val == null) return 0.5;
          return smallGroupPercentileRank(colValues, val);
        });
      })
    : selectedMetrics.map((_, i) =>
        normaliseMetric(users.map((u) => u.values[i]))
      );


  const scoreFn = scoringStrategies[method] ?? scoringStrategies.Default;

  return users.map((user, idx) => {
    const scales = metricsValues.map((col) => col[idx]);
    const score = scoreFn(scales, idx, users, selectedMetrics);
    return { name: user.name, score: Math.round(score * 100) / 100 };
  });
}

//
// ---------- Main exported function ----------
//

export async function getScaledResults(
  repoData: SerializableRepoData,
  config: ScalingConfig,
  repoUrl: string
): Promise<UserScalingSummary[]> {
  // Fetch all alias configs for current user
  const aliasConfig = await Meteor.callAsync("aliasConfigs.getAllForOwner").catch(() => null) as { aliases: StudentAlias[] }[] | null; // should never be null

  // Apply mapping if config exists
  const mappedData: SerializableRepoData = aliasConfig?.length
    ? applyAliasMapping(repoData, createAliasMapping(aliasConfig[0].aliases))
    : repoData;

  // Build a lookup map from officialName -> alias object
  const aliasMap = new Map<string, StudentAlias>(
    (aliasConfig?.[0]?.aliases ?? []).map((a: StudentAlias) => [a.officialName, a])
  );

  // Enrich contributors locally using the aliasMap
  const updatedContributors: SerialisableMapObject<string, ContributorValueWithAliases>[] =
    mappedData.contributors.map((c) => {
      const match = aliasMap.get(c.key);
      const emails = [...new Set([...c.value.emails, ...(match?.emails ?? [])])];
      const aliases = [
        ...emails.map((email) => ({ username: c.key, email })),
        ...(match?.gitUsernames ?? [])
          .filter((u) => u !== c.key)
          .map((u) => ({ username: u, email: null })),
      ];

      return {
        key: c.key,
        value: {
          name: c.key,
          emails,
          aliases,
        } as ContributorValueWithAliases,
      };
    });

  // Compute scaled scores
  const scaledUsers = await scaleUsers(repoUrl, config);

  // Build final results
  return updatedContributors.map((c) => ({
    name: c.key,
    aliases: c.value.aliases,
    finalGrade: null,
    scale: scaledUsers.find((u) => u.name === c.key)?.score ?? 0,
  }));
}

