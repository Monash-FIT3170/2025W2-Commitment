// server/metrics_retrieval.ts

import * as Meteor from "meteor/meteor";
import { Subject } from "rxjs";
import { RepositoryData } from "./commitment_api/types";
import { metricsFunctions, MetricFn } from "./repo_metrics";
import { getRepoData } from "./caching"; 

function resolveMetricFns(metricNames: string[]): MetricFn[] {
  const fns: MetricFn[] = [];
  for (const name of metricNames) {
    const fn = metricsFunctions.get(name);
    if (!fn) {
      throw new Meteor.Error("MetricNotFound", `Unknown metric: "${name}".`);
    }
    fns.push(fn);
  }
  return fns;
}

Meteor.methods({
  /**
   * Get a single metric by name
   * EXAMPLE: Meteor.call("metrics.getMetricFromRepo", repoUrl, "locLineData", cb)
   */
  async "metrics.getMetricFromRepo"(repoUrl: string, metricName: string) {
    if (typeof repoUrl !== "string" || !repoUrl.trim()) {
      throw new Meteor.Error("BadRequest", "repoUrl must be a non-empty string.");
    }
    if (typeof metricName !== "string" || !metricName.trim()) {
      throw new Meteor.Error("BadRequest", "metricName must be a non-empty string.");
    }

    const notifier = new Subject<string>();
    const repoData: RepositoryData = await getRepoData(repoUrl, notifier);

    const fn = metricsFunctions.get(metricName);
    if (!fn) throw new Meteor.Error("MetricNotFound", `Unknown metric: "${metricName}".`);

    return fn(repoData);
  },

  /**
   * Get multiple metrics at once, keyed by requested name.
   * Usage (client):
   * Meteor.call("metrics.getMetricsFromRepo", repoUrl, ["branches","users","totalLocData"], cb)
   */
  async "metrics.getMetricsFromRepo"(repoUrl: string, metricNames: string[]) {
    if (typeof repoUrl !== "string" || !repoUrl.trim()) {
      throw new Meteor.Error("BadRequest", "repoUrl must be a non-empty string.");
    }
    if (!Array.isArray(metricNames) || metricNames.length === 0) {
      throw new Meteor.Error("BadRequest", "metricNames must be a non-empty array of strings.");
    }

    const notifier = new Subject<string>();
    const repoData: RepositoryData = await getRepoData(repoUrl, notifier);

    const fns = resolveMetricFns(metricNames);

    const results: Record<string, any> = {};
    fns.forEach((fn, idx) => {
      const name = metricNames[idx];
      results[name] = fn(repoData);
    });

    return results;
  },
});




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