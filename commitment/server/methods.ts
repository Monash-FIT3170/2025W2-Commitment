import { Meteor } from "meteor/meteor";
import { getFilteredRepoDataServer } from "./filter";
import {
  SerializableRepoData,
  FilteredData,
  AnalyticsData,
  Metadata,
  MetricsData,
  Selections,
} from "/imports/api/types";
import { start } from "repl";
import { getAllMetrics } from "./repo_metrics";

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
    contributor
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

    console.log("Right before we call getFilteredRepoData in .getFilteredData", contributor)

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
  }: {
    repoUrl: string;
    startDate?: Date;
    endDate?: Date;
    branch?: string;
    contributors?: string[];
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
      selectedDateRange: {
        from: startDate || metadata.dateRange.from,
        to: endDate || metadata.dateRange.to,
      },
    };


    console.log("Right before we call te getFilteredData in getAnalytics data: ", selections.selectedContributors)
    const filteredRepo: FilteredData = await Meteor.callAsync(
      "repo.getFilteredData",
      {
        repoUrl,
        startDate: selections.selectedDateRange.from,
        endDate: selections.selectedDateRange.to,
        branch: selections.selectedBranch,
        contributors: selections.selectedContributors,
      }
    );

    console.log("Filtered Repo Data:", filteredRepo);
    console.log("Contributors:", filteredRepo.repositoryData.contributors);
    console.log("Deeper look:", filteredRepo.repositoryData.allCommits);

    const metricsData: MetricsData = await getAllMetrics(filteredRepo);

    // NOW WE DO STUFF WITH THE FILTERED REPO TO GET METRICS
    const returnData: AnalyticsData = {
      metadata,
      selections,
      metrics: metricsData,
    };

    return returnData;
  },
});
