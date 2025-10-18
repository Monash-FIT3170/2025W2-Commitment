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

/**
SCALING METHODS

Percentiles	- Rank contributors relative to others, average across metrics	

Mean +/- Std - Average metrics + some extra credit for spread

Quartiles - Use median metric value

Compact Scaling - Smooth values using tanh, floor 0.5 ceiling 1.2 (tries to give higher marks and a bit of leniancy)

Default	- Simple mean of normalized metrics	
 */

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

//
// ---------- User building ----------
//
/**
 * Will essentially convert raw metric data to structured array in the format {name:... , values:{12,13,...}}
 * @param allMetrics 
 * @param selectedMetrics 
 * @returns the built users containing their relevant metrics
 */
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


/**
 * This method stores the mathematical logic for each scaling method.
 */
const scoringStrategies: Record<string, ScoreFn> = {
    Percentiles: (_, idx, users, selectedMetrics) => {
        const percentileScores = selectedMetrics.map((_metric, colIdx) => {
            const colValues = users
                .map((u) => u.values[colIdx])
                .filter((v): v is number => v !== null);

            return users.map((u) => {
                const val = u.values[colIdx];
                if (val == null) return 0.5;
                
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

    "Compact Scaling": (scales: number[], idx: number) => {
    // Filter only finite numbers
    const validScales = scales.filter((v) => Number.isFinite(v));
    if (!validScales.length) return 1; // fallback if all invalid

    const min = Math.min(...validScales);
    const max = Math.max(...validScales);

    if (min === max) return 1; // all same -> neutral 1

    const value = scales[idx];
    if (!Number.isFinite(value)) return 1; // fallback for this user

    // Linear mapping from min..max -> 0.5..1.2
    const normalized = (value - min) / (max - min);
    return 0.5 + normalized * 0.7; // 0.5 -> min, 1.2 -> max
},




    Default: (scales) => scales.reduce((a, b) => a + b, 0) / scales.length, // just calculates the mean (average) of the user’s normalized metric values
};

//
// ---------- Scaling ----------
//

const DEFAULT_METRICS = ["Total No. Commits", "LOC", "LOC Per Commit", "Commits Per Day", "Compact Scaling"];

/**
 * Builds a matrix of users and their metrics and then uses the chosen scoring method to compute their final scale.
 It rounds to 2 decimal places for the scale
 * @param repoUrl 
 * @param config 
 * @returns array of {name, scale} 
 */
export async function scaleUsers(repoUrl: string, config: ScalingConfig) {
    const allMetrics = await Meteor.callAsync("repo.getAllMetrics", { repoUrl }) as AllMetricsData; // ✅ casting is fine here

    const selectedMetrics = config.metrics?.length ? config.metrics : DEFAULT_METRICS;
    const method = config.method ?? "Percentiles";

    const users = buildUsers(allMetrics, selectedMetrics);

    if (!users.length) return [];

    const metricsValues = selectedMetrics.map((_, i) =>
        users.map((u) => (Number.isFinite(u.values[i]) ? u.values[i]! : 0))
    );



    const scoreFn = scoringStrategies[method] ?? scoringStrategies.Default;

    const rawScores = users.map((_, idx) => {
        const scales = metricsValues.map((col) => col[idx]);
        return scoreFn(scales, idx, users, selectedMetrics);
    });


    console.log("RAW SCORES", rawScores)

    const mean = rawScores.length ? rawScores.reduce((a, b) => a + b, 0) / rawScores.length : 0;

    const std = rawScores.length
        ? Math.sqrt(rawScores.reduce((sum, x) => sum + (x - mean) ** 2, 0) / rawScores.length)
        : 0;

    const safeStd = std || 1; // if std = 0, assume 1 so thresholds work

    function normalise_scale(score: number) {

    const diff = score - mean;

    if (diff <= -3 * safeStd) return 0;
    if (diff <= -2 * safeStd) return 0.5;
    if (diff <= -1 * safeStd) return 0.9;
    if (diff <= 1.2 * safeStd) return 1;
    if (diff <= 3 * safeStd) return 1.1;
    return 1.2;

    }

    return users.map((user, i) => ({
    name: user.name,
    score: Math.round(normalise_scale(rawScores[i]) * 100) / 100,
    }));
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

// Meteor.startup(async () => {
//   console.log("COMPACT SCALING TEST");

//   const fakeMetrics = {
//     alice: { "Total No. Commits": 10, LOC: 200, "LOC Per Commit": 20, "Commits Per Day": 1 },
//     bob: { "Total No. Commits": 30, LOC: 400, "LOC Per Commit": 20, "Commits Per Day": 2 },
//     boeb: { "Total No. Commits": 50, LOC: 400, "LOC Per Commit": 240, "Commits Per Day": 3 },
//     boba: { "Total No. Commits": 1000, LOC: 450, "LOC Per Commit": 40, "Commits Per Day": 5 },
//     charlie: { "Total No. Commits": 5000, LOC: 6000, "LOC Per Commit": 3000, "Commits Per Day": 30 },

//   };

// //     "Percentiles",
// //     "Mean +/- Std",
// //     "Quartiles",
// //     "Compact Scaling",

//   const config: ScalingConfig = {
//     method: "Compact Scaling",
//     metrics: ["Total No. Commits", "LOC"], // match keys in fakeMetrics
//   };

//   const selectedMetrics = config.metrics;
//   const users = Object.entries(fakeMetrics).map(([name, metrics]) => ({
//     name,
//     values: selectedMetrics.map((m) => metrics[m as keyof typeof metrics] ?? null),
//   }));

//   const metricsValues = selectedMetrics.map((_, i) =>
//   users.map((u) => u.values[i] as number) // raw numbers
// );


//   const scoreFn = scoringStrategies[config.method] ?? scoringStrategies.Default;

//   const rawScores = users.map((_, idx) => {
//     const scales = metricsValues.map((col) => col[idx]);
//     return scoreFn(scales, idx, users, selectedMetrics);
//   });

//   const mean = rawScores.reduce((a, b) => a + b, 0) / rawScores.length;
//   const std = Math.sqrt(rawScores.reduce((sum, x) => sum + (x - mean) ** 2, 0) / rawScores.length);

//   function normalise_scale(score: number) {

//     const diff = score - mean;
//     console.log("MEAN", mean)

//     console.log("THIS IS diff", diff , "THIS IS std", 1.3*std);

//     if (diff <= -3 * std) return 0;
//     if (diff <= -2 * std) return 0.5;
//     if (diff <= -1 * std) return 0.9;
//     if (diff <= 1.2 * std) return 1;
//     if (diff <= 3 * std) return 1.1;
//     return 1.2;
//   }

//   const scaledUsers = users.map((user, i) => ({
//     name: user.name,
//     score: Math.round(normalise_scale(rawScores[i]) * 100) / 100,
//   }));

//   console.log("THE DUMMY USERS:", scaledUsers);
// });