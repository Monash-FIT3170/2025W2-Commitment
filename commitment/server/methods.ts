import { Meteor } from "meteor/meteor";
import { Subject } from "rxjs";

import { getFilteredRepoDataServer } from "./filter";
import { tryFromDatabaseSerialised } from "./api/caching";
import {
  SerializableRepoData,
  FilteredData,
  AnalyticsData,
  Metadata,
  MetricsData,
  Selections,
  AllMetricsData,
  MetricType,
  RepositoryData,
} from "../imports/api/types";

import { getContributors } from "./helper_functions";

import { getAllGraphData, getAllMetricsFromData } from "./repo_metrics";
import { applyAliasMappingIfNeeded } from "./alias_mapping";
import { getSerialisedRepoData } from "./api/fetch_repo";
import { getScaledResults } from "./ScalingFunctions";
import { ScalingConfig } from "/imports/ui/components/scaling/ScalingConfigForm";

Meteor.methods({
  /**
   * Get filtered repository data from the server
   * @param params.daysBack Number of days to look back (default: 7)
   * @param params.branch Branch to filter (optional)
   * @param params.contributor Contributor to filter (optional)
   * @returns FilteredData structure
   */
  async "repo.getFilteredData"({
    repoUrl,
    startDate,
    endDate,
    branch,
    contributor,
  }: {
    repoUrl: string; // pass the URl from the frontend
    startDate: Date;
    endDate: Date;
    branch?: string;
    contributor?: string[];
  }): Promise<FilteredData> {
    // TODO do type checks here if needed

    // Get full repository data from db (fetches from API if not updated or in the database)
    const repo: SerializableRepoData = await getSerialisedRepoData(repoUrl, null);

    // Apply alias mapping if user has config
    const mappedRepo = await applyAliasMappingIfNeeded(repo, this.userId || "");

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
  },

  async "repo.getMetadata"(repoUrl: string): Promise<Metadata> {
    // Get full repository data from db
    const repo: SerializableRepoData = await getSerialisedRepoData(repoUrl, null);

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
      selectedContributors:
        !contributors || contributors.length === 0 ? metadata.contributors : contributors,
      selectedMetrics: metric,
      selectedDateRange: {
        from: startDate || metadata.dateRange.from,
        to: endDate || metadata.dateRange.to,
      },
    };

    const filteredRepo: FilteredData = await Meteor.callAsync("repo.getFilteredData", {
      repoUrl,
      startDate: selections.selectedDateRange.from,
      endDate: selections.selectedDateRange.to,
      branch: selections.selectedBranch,
      contributor: selections.selectedContributors,
    });

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
    const repo: SerializableRepoData = await getSerialisedRepoData(repoUrl, null);

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
