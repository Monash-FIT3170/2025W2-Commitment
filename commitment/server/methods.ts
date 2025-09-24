import { Meteor } from "meteor/meteor";
import { Subject } from "rxjs";

import { getFilteredRepoDataServer } from "./filter";
import { tryFromDatabaseSerialised } from "./caching";
import {
  SerializableRepoData,
  FilteredData,
  AnalyticsData,
  Metadata,
  MetricsData,
  Selections,
  AllMetricsData,
  MetricType,
} from "../imports/api/types";

import { getAllGraphData, getAllMetricsFromData } from "./repo_metrics";
import { applyAliasMappingIfNeeded } from "./alias_mapping";
import { getScaledResults } from "./ScalingFunctions";
import { ScalingConfig } from "/imports/ui/components/scaling/ScalingConfigForm";
import { spawn } from "child_process";

export async function getFilteredRepoData(  repoUrl:string, startDate:Date, endDate:Date, branch:string, contributor:string[]):Promise<FilteredData>{
    // Get full repository data from db
    const repo = await Meteor.callAsync("repoCollection.getData", repoUrl) as SerializableRepoData;

    // Apply alias mapping if user has config
    const userId = Meteor.userId();
    const mappedRepo = await applyAliasMappingIfNeeded(repo, userId || "");

    // Apply filtering
    const filteredData = getFilteredRepoDataServer(
      repoUrl,
      startDate,
      endDate,
      mappedRepo,
      branch,
      contributor
    );
    return filteredData;
}

Meteor.methods({
  /**
   * Check if a repository exists and is accessible
   * @param repoUrl The repository URL to check
   * @returns Promise<boolean> True if repository exists and is accessible
   */
  async "repo.checkExists"(repoUrl: string): Promise<boolean> {
    return new Promise((resolve) => {
      const git = spawn('git', ['ls-remote', repoUrl], {
        stdio: ['ignore', 'pipe', 'pipe'],
        env: { ...process.env, GIT_TERMINAL_PROMPT: '0' }
      });
      
      git.on('close', (code) => {
        resolve(code === 0);
      });
      
      git.on('error', () => {
        resolve(false);
      });
    });
  },

  async "repo.getMetadata"(repoUrl: string): Promise<Metadata> {
    // Get full repository data from db
    const repo: SerializableRepoData = await Meteor.callAsync("repoCollection.getData", repoUrl);

    // Apply alias mapping if user has config
    const mappedRepo = await applyAliasMappingIfNeeded(repo, this.userId || "");

    return {
      repoUrl,
      repoName: mappedRepo.name,
      branches: mappedRepo.branches.map((b) => b.branchName),
      contributors: mappedRepo.contributors.map((c) => c.key),
      dateRange: {
        from: new Date(
          Math.min(...mappedRepo.allCommits.map((c) => new Date(c.value.timestamp).getTime()))
        ),
        to: new Date(),
      },
    };
  },

  async "repo.getAnalyticsData"({
    repoUrl,
    startDate,
    endDate,
    branch,
    contributors,
    metric,
  }: {
    repoUrl: string;
    startDate?: Date;
    endDate?: Date;
    branch?: string;
    contributors?: string[];
    metric: MetricType;
  }): Promise<AnalyticsData> {
    /**
     * Get Repo Metadata first (contributors, branches, date range) etc
     *
     * Then get the filtered data depending on the parameters that have been passed
     *
     * Run our metrics functions on the filtered data
     *
     * Return the full AnalyticsData structure
     */

    // Get project metadata
    const metadata: Metadata = await Meteor.callAsync("repo.getMetadata", repoUrl);

    const selections: Selections = {
      selectedBranch:
        branch ??
        (metadata.branches.includes("main")
          ? "main"
          : metadata.branches.includes("master")
          ? "master"
          : metadata.branches[0]),
      selectedContributors: contributors ?? [],
      selectedMetrics: metric,
      selectedDateRange: {
        from: startDate || metadata.dateRange.from,
        to: endDate || metadata.dateRange.to,
      },
    };

    // const filteredRepo: FilteredData = await Meteor.callAsync("repo.getFilteredData", {
    //   repoUrl,
    //   startDate: selections.selectedDateRange.from,
    //   endDate: selections.selectedDateRange.to,
    //   branch: selections.selectedBranch,
    //   contributor: selections.selectedContributors,
    // });
    const filteredRepo: FilteredData = await getFilteredRepoData(
      repoUrl,
      selections.selectedDateRange.from,
      selections.selectedDateRange.to,
      selections.selectedBranch,
      selections.selectedContributors
    );
    const metricsData: MetricsData = await getAllGraphData(filteredRepo, metric);

    // NOW WE DO STUFF WITH THE FILTERED REPO TO GET the specific metric!!
    const returnData: AnalyticsData = {
      metadata,
      selections,
      metrics: metricsData,
    };

    return returnData;
  },

  /**
   * Get all metrics for a repository with alias mapping applied
   * @param param0
   * @returns
   */
  async "repo.getAllMetrics"({ repoUrl }: { repoUrl: string }): Promise<AllMetricsData> {
    // Get repository data and apply alias mapping
    const repo: SerializableRepoData = await Meteor.callAsync("repoCollection.getData", repoUrl);

    const mappedRepo = await applyAliasMappingIfNeeded(repo, this.userId || "");

    // Use the mapped data for metrics calculation
    return await getAllMetricsFromData(mappedRepo);
  },

  async getScalingResults(data: ScalingConfig, repoUrl: string) {
    const n = new Subject<string>();
    return getScaledResults(
      await tryFromDatabaseSerialised(repoUrl, n),
      data,
      repoUrl,
      "" // null string for now as Yoonus is TODO fix this
    );
  },

});
