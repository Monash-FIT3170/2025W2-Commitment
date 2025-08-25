import { Subject } from "rxjs";

import { RepositoryData } from "../imports/api/types";
import { metricsFunctions, MetricFn } from "../server/repo_metrics";
import { getRepoData } from "../server/fetch_repo";



Meteor.methods({
  /**
   * Get a single metric by name
   * EXAMPLE: Meteor.call("metrics.getMetricFromRepo", repoUrl, "getBranchNames", cb)
   */
  async "metrics.getMetricFromRepo"(repoUrl: string, metricName: string) {
    return processMetrics(repoUrl, [metricName])
      .then((r) => r[0])
      .catch((e: Error) => {
          throw new Meteor.error("MetricsError", e.message)
        });;
  },

  /**
   * Get multiple metrics at once, keyed by requested name.
   * Usage (client):
   * Meteor.call("metrics.getMetricsFromRepo", repoUrl, ["branches","users","totalLocData"], cb)
   */
  async "metrics.getMetricsFromRepo"(repoUrl: string, metricNames: string[]) {
    return processMetrics(repoUrl, metricNames)
      .catch((e: Error) => {
        throw new Meteor.error("MetricsError", e.message)
      });
  },
});

const resolveMetricFns = (metricNames: string[]): MetricFn[] => {
  const res = metricNames.map((name) => {
    const r = metricsFunctions.get(name);
    return r !== null ? r : name;
  });
  const err = res.filter((r) => typeof r === "string");
  if (err.length > 0) throw Error(`Unknown metrics: "${err}".`);
  return res as MetricFn[];
};

const processMetrics = async (repoUrl: string, metricNames: string[]) => {
  if (typeof repoUrl !== "string" || !repoUrl.trim()) {
    throw Error("repoUrl must be a non-empty string.");
  }
  if (!Array.isArray(metricNames) || metricNames.length === 0) {
    throw Error("metricNames must be a non-empty array of strings.");
  }

  const notifier = new Subject<string>();
  const repoData: RepositoryData = await getRepoData(repoUrl, notifier);

  const fns = resolveMetricFns(metricNames);
  return fns.map((fn) => fn(repoData));
};

// const testFunction = (data: RepositoryData): void => {}

// const metricsFunctions = new Map<string, <T>(data: RepositoryData) => T>([
//     ["something", testFunction]
// ])

// Meteor.methods({
//     async "getMetricFromRepo" <T>(repoUrl: string, metricFunctionName: string) {

//         const repo = await getRepoData(repoUrl, new Subject<string>())
//         const res = metricsFunctions.get(metricFunctionName)

//         if (res == null) {
//             return null
//         }

//         return res(repo)
//     }
// })

// very rough example on how front end can call multiple metrics to use. May or may not be correct
// [loc, auth_per_commit] = Meteor.call("getMetricFromRepo", "test_repo", ["loc", "auth_per_commit"])
