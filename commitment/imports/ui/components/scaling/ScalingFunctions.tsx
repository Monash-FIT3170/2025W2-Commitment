import {
  RepositoryData,
  CommitData,
  ContributorData,
  UserScalingSummary,
} from "@server/commitment_api/types";

//The many many helper functions!

/**
 * This function is used to compute the percentile of a value
 *
 * @param value the value you want to find the percentile of
 * @param allValues the values to compare this value to
 * @returns the percentile
 */
function computePercentile(value: number, allValues: number[]): number {
  const sorted = [...allValues].sort((a, b) => a - b);
  const rank = sorted.findIndex((v) => v >= value);
  return (rank / sorted.length) * 100;
}

/**
 *
 * @param percentile
 * @returns gives the scaling percentile
 */
function scalePercentile(percentile: number): number {
  if (percentile < 20) return 0;
  if (percentile < 35) return 0.2;
  if (percentile < 50) return 0.4;
  if (percentile < 65) return 0.6;
  if (percentile < 80) return 0.8;
  if (percentile < 95) return 1.0;
  return 1.2;
}

/**
 * The function maps the number of standard deviations from the mean (z)
 * to a fixed scale from 0 to 1.2, in increments of 0.2. This can be used
 * to normalize metric values for grading or scaling purposes.
 *
 * @param z - The z-score (number of standard deviations from the mean)
 * @returns A scaling score from 0 to 1.2 in 0.2 increments
 */
function scaleZScore(z: number): number {
  if (z < -2) return 0;
  if (z < -1) return 0.2;
  if (z < 0) return 0.4;
  if (z < 1) return 0.6;
  if (z < 2) return 0.8;
  if (z < 3) return 1.0;
  return 1.2;
}

/**
 * Combines multiple metric scores into a single scaling score.
 *
 * If weights are provided, calculates a weighted average; otherwise, calculates
 * the simple mean. The result is rounded to the nearest 0.2 increment (0, 0.2, ..., 1.2)
 * for consistency with the scaling system.
 *
 * @param scores - An array of individual metric scores
 * @param weights - Optional array of weights corresponding to each score
 * @returns A single combined score rounded to the nearest 0.2 increment
 */
function combineScores(scores: number[], weights?: number[]): number {
  if (weights && weights.length === scores.length) {
    const weightedSum = scores.reduce((sum, s, i) => sum + s * weights[i], 0);
    const normalized = weightedSum / weights.reduce((a, b) => a + b, 0);
    return Math.round(normalized * 5) / 5; // 0.2 increments
  } else {
    const mean = scores.reduce((a, b) => a + b, 0) / scores.length;
    return Math.round(mean * 5) / 5;
  }
}

// ---------- Metric Extraction Functions ----------
interface ContributorMetrics {
  totalCommits: number;
  linesOfCode: number;
  locPerCommit: number;
  commitsPerDay: number;
}

/**
 * Aggregates commit data to compute metrics per contributor.
 *
 * For each contributor, calculates:
 * - totalCommits: total number of commits made
 * - linesOfCode: total number of characters added across all files in all commits
 * - locPerCommit: average lines of code per commit
 * - commitsPerDay: average number of commits per day based on first and last commit timestamps
 *
 * @param commits - An array of CommitData objects representing all commits in the repository
 * @returns A Map where each key is a contributor's name and each value is a ContributorMetrics object
 *
 * @example
 * const metricsMap = extractContributorMetrics(allCommits);
 * const aliceMetrics = metricsMap.get("Alice");
 * console.log(aliceMetrics.totalCommits);
 */
function extractContributorMetrics(
  commits: CommitData[]
): Map<string, ContributorMetrics> {
  const contributorMap = new Map<string, ContributorMetrics>();

  // aggregate metrics
  commits.forEach((c) => {
    const name = c.contributorName;
    const loc = c.fileData.reduce((sum, f) => sum + f.file.contents.length, 0);

    const existing = contributorMap.get(name) || {
      totalCommits: 0,
      linesOfCode: 0,
      locPerCommit: 0,
      commitsPerDay: 0,
    };

    existing.totalCommits += 1;
    existing.linesOfCode += loc;
    contributorMap.set(name, existing);
  });

  // compute locPerCommit and commitsPerDay (assuming timestamp differences for simplicity)
  contributorMap.forEach((metrics, name) => {
    metrics.locPerCommit =
      metrics.totalCommits > 0 ? metrics.linesOfCode / metrics.totalCommits : 0;
    // simple commits/day: assuming first-last timestamp
    const userCommits = commits
      .filter((c) => c.contributorName === name)
      .sort((a, b) => a.timestamp.getTime() - b.timestamp.getTime());
    if (userCommits.length > 1) {
      const days =
        (userCommits[userCommits.length - 1].timestamp.getTime() -
          userCommits[0].timestamp.getTime()) /
          (1000 * 60 * 60 * 24) || 1;
      metrics.commitsPerDay = metrics.totalCommits / days;
    } else metrics.commitsPerDay = metrics.totalCommits;
  });

  return contributorMap;
}

// ---------- Scaling ----------
type MetricKey = keyof ContributorMetrics;

export interface ScalingOptions {
  metrics: MetricKey[];
  weights?: number[];
  strategy: "percentiles" | "zScore"; //ADD MORE STRATEGIES!
}

export function scaleContributors(
  commits: CommitData[],
  options: ScalingOptions,
  contributors: ContributorData[]
): UserScalingSummary[] {
  const metricsMap = extractContributorMetrics(commits);

  // prepare arrays for normalization
  const metricValues: Record<MetricKey, number[]> = {} as any;
  options.metrics.forEach((metric) => {
    metricValues[metric] = Array.from(metricsMap.values()).map(
      (m) => m[metric]
    );
  });

  return contributors.map((c) => {
    const metrics = metricsMap.get(c.name) || {
      totalCommits: 0,
      linesOfCode: 0,
      locPerCommit: 0,
      commitsPerDay: 0,
    };

    const scores = options.metrics.map((metric) => {
      const value = metrics[metric];
      const allValues = metricValues[metric];
      if (options.strategy === "percentiles") {
        const perc = computePercentile(value, allValues);
        return scalePercentile(perc);
      } else {
        // zScore
        const mean = allValues.reduce((a, b) => a + b, 0) / allValues.length;
        const sd =
          Math.sqrt(
            allValues.reduce((sum, v) => sum + (v - mean) ** 2, 0) /
              allValues.length
          ) || 1;
        const z = (value - mean) / sd;
        return scaleZScore(z);
      }
    });

    const scale = combineScores(scores, options.weights);

    return {
      name: c.name,
      aliases: c.emails.map((email) => ({ username: c.name, email })),
      finalGrade: scale, // can adjust mapping to grade if u want
      scale,
    };
  });
}
