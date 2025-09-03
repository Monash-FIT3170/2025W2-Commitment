import { Meteor } from "meteor/meteor";
import { getFilteredRepoDataServer } from "./filter";
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
} from "/imports/api/types";
import { getAllGraphData, getMetricString, getAllMetrics, getContributors, getUnfilteredData, setsUnfilteredData } from "./repo_metrics";
import { getRepoData } from "./fetch_repo";
import { deserializeRepoData, serializeRepoData } from "/imports/api/serialisation";
import { getScaledResults } from "./ScalingFunctions";
import { ScalingConfig } from "/imports/ui/components/scaling/ScalingConfigForm";
import { useLocation } from "react-router-dom";
import { serialize } from "v8";
import { createDropdownMenuScope } from "@radix-ui/react-dropdown-menu";

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
    // Get full repository data from db
    const repo: SerializableRepoData = await Meteor.callAsync(
      "repoCollection.getData",
      repoUrl
    );

    // Apply filtering
    const filteredData = getFilteredRepoDataServer(
      repoUrl,
      startDate,
      endDate,
      repo,
      branch,
      contributor
    );
    return filteredData;
  },

  async "repo.getMetadata"(repoUrl: string): Promise<Metadata> {
    // Get full repository data from db
    const repo: SerializableRepoData = await Meteor.callAsync(
      "repoCollection.getData",
      repoUrl
    );

    return {
      repoUrl,
      repoName: repo.name,
      branches: repo.branches.map((b) => b.branchName),
      contributors: repo.contributors.map((c) => c.key),
      dateRange: {
        from: new Date(
          Math.min(
            ...repo.allCommits.map((c) => new Date(c.value.timestamp).getTime())
          )
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
    metric
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
    const metadata: Metadata = await Meteor.callAsync(
      "repo.getMetadata",
      repoUrl
    );

    const selections: Selections = {
      selectedBranch:
        branch ??
        (metadata.branches.includes("main")
          ? "main"
          : metadata.branches.includes("master")
          ? "master"
          : metadata.branches[0]),
      selectedContributors:
        !contributors || contributors.length === 0
          ? metadata.contributors
          : contributors,
      selectedMetrics: metric ,
      selectedDateRange: {
        from: startDate || metadata.dateRange.from,
        to: endDate || metadata.dateRange.to,
      },
    };

    const filteredRepo: FilteredData = await Meteor.callAsync(
      "repo.getFilteredData",
      {
        repoUrl,
        startDate: selections.selectedDateRange.from,
        endDate: selections.selectedDateRange.to,
        branch: selections.selectedBranch,
        contributor: selections.selectedContributors,
      }
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
   * 
   * @param param0 
   * @returns 
   */
  async "repo.getAllMetrics"({repoUrl}: {repoUrl: string}): Promise<AllMetricsData> {
    const repo: SerializableRepoData = await Meteor.callAsync(
      "repoCollection.getData",
      repoUrl
    );
    console.log("repo data: ", repo); 

    console.log("commithash for main in repoData: ", repo.branches.find(b => b.branchName === "main")?.commitHashes);
    const metadata: Metadata = await Meteor.callAsync(
      "repo.getMetadata",
      repoUrl
    );
    console.log("meta data: ", metadata)
    // set the branch to filteredBranch to "main " or "master"

    const filteredBranch = metadata.branches.includes("main")
      ? "main"
      : metadata.branches.includes("master")
      ? "master"
      : metadata.branches[0];

    console.log("Filtered branches: ", filteredBranch);
    // get all the filteredRepo Data - all the data from Main or Master Branch
    const filteredRepo: FilteredData = await Meteor.callAsync(
      "repo.getFilteredData",
      {
        repoUrl,
        startDate: metadata.dateRange.from,
        endDate: metadata.dateRange.to,
        branch: filteredBranch,
        contributor: metadata.contributors,
      }
    );

    const serializedFilteredData = filteredRepo.repositoryData; 
    console.log("filtered data: ", serializedFilteredData); 
    return await getAllMetrics(serializedFilteredData);
  },

async "getScalingResults"(data:ScalingConfig,repoUrl:string) {

    setsUnfilteredData(repoUrl);

    const repoData:SerializableRepoData = await getUnfilteredData();

    const result = await getScaledResults(repoData,data,repoUrl)

    return result
}
 
});
