import { Meteor } from "meteor/meteor";
import { getFilteredRepoDataServer } from "./filter";
import {
  SerializableRepoData,
  FilteredData,
  AnalyticsData,
} from "/imports/api/types";
import { start } from "repl";

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

  async "repo.getMetadata"(
    repoUrl: string
  ): Promise<AnalyticsData["metadata"]> {
    // Get full repository data from db
    const repo: SerializableRepoData = await Meteor.callAsync(
      "repoCollection.getData",
      repoUrl
    );

    return {
      repoUrl: repo.repoUrl,
      branch: repo.defaultBranch,
      repoName: repo.repoName,
      branches: repo.branches.map((b) => b.branchName),
      contributors: repo.contributors.map((c) => c.key),
      dateRange: {
        start: new Date(
          Math.min(
            ...repo.allCommits.map((c) => new Date(c.value.timestamp).getTime())
          )
        ),
        end: new Date(
          Math.max(
            ...repo.allCommits.map((c) => new Date(c.value.timestamp).getTime())
          )
        ),
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
    const metadata: AnalyticsData["metadata"] = await Meteor.callAsync(
      "repo.getMetadata",
      repoUrl
    );

    // Update metadata with filter date range
    metadata.filterRange = {
      start: startDate || metadata.dateRange.start,
      end: endDate || metadata.dateRange.end,
    };

    const filteredRepo: FilteredData = await Meteor.callAsync(
      "repo.getFilteredData",
      {
        repoUrl,
        startDate: metadata.filterRange.start,
        endDate: metadata.filterRange.end,
        branch:
          branch ??
          (metadata.branches.includes("main")
            ? "main"
            : metadata.branches.includes("master")
            ? "master"
            : metadata.branches[0]),
        contributors: contributors ? contributors : metadata.contributors,
      }
    );

    // NOW WE DO STUFF WITH THE FILTERED REPO TO GET METRICS
    const data: AnalyticsData;

    return data;
  },
});
