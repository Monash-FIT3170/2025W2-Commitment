import { Meteor } from "meteor/meteor";
import { getFilteredRepoDataServer } from "./filter";
import { SerializableRepoData, FilteredData, RepositoryData} from "/imports/api/types";
import { tryFromDatabase } from "./caching";
import { Subject } from "rxjs";
import { Sub } from "@radix-ui/react-navigation-menu";
import { getRepoData } from "./fetch_repo";

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
  const repo: SerializableRepoData = await Meteor.callAsync('repoCollection.getData');
  console.log("repoCollection.getData:", repo);
  console.log("checking type of commits,  contributors: ", typeof repo.allCommits, typeof repo.contributors);
    // Apply filtering
  const filteredData = getFilteredRepoDataServer(
    repo,
    daysBack,
    branch,
    contributor
  );
    console.log("Filtered Data:", filteredData);
    return filteredData;
  },
});