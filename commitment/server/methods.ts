import { Meteor } from "meteor/meteor";
import { getFilteredRepoDataServer } from "./filter";
import { RepositoryData, FilteredData} from "/imports/api/types";

Meteor.methods({
  /**
   * Get filtered repository data from the server
   * @param params.daysBack Number of days to look back (default: 7)
   * @param params.branch Branch to filter (optional)
   * @param params.contributor Contributor to filter (optional)
   * @returns FilteredData structure
   */
  async "repo.getFilteredData"({
    daysBack,
    branch,
    contributor,
    repoUrl,
  }: {
    daysBack: number;
    branch?: string;
    contributor?: string;
    repoUrl: string; // pass the URl from the frontend 
  }): Promise<FilteredData> {
    // Get full repository data from db
    const repo: RepositoryData = await Meteor.call("repoCollection.getData", repoUrl);

    // Apply filtering
    const filteredData = getFilteredRepoDataServer(
      repo,
      daysBack,
      branch,
      contributor
    );

    return filteredData;
  },
});