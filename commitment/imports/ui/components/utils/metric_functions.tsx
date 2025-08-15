import { Meteor } from 'meteor/meteor'
import { RepositoryData, CommitData, ContributorData, BranchData } from '/server/commitment_api/types';
import { Subject } from "rxjs";
import { SerializableRepoData, deserializeRepoData } from './helper';
// import { StringDecoder } from 'string_decoder';



// Functions to fetch repository data from the RepoCollection in a clean way
// TO DO: add more functions tailoured to MetricPage needs - perhaps a new file ? 
// to do: remove getrepoData as this function no longer necessary (used in data_filter) function :
export const getRepoData = async (url: string, notifier: Subject<string>): Promise<RepositoryData> => {
    console.log("this method called with url:", url);
    return new Promise((resolve, reject) => {
        notifier.next('Fetching repo data...');
        // this is the NEW ERROR FOR NOW because i couldn't figure out how to transition to the metricspage correctly 
        // Meteor.call(repoCollection.getData) is what should be called. 
        Meteor.call('repoCollection.getData', url, (err: Error, result: SerializableRepoData) => {
            console.log("in getrepodata")
            if (err) {
                console.log(`Error fetching repo data for URL ${url}:`, err);
                notifier.next(`Error fetching repo data: ${err.message}`);
                reject(err);
            } else {
                notifier.next('Repo data fetched successfully.');
                console.log("In metric functions:", result);
                console.log(typeof result.allCommits)
                console.log(result.allCommits)

                const dRepo = deserializeRepoData(result);

                resolve(dRepo);
            }
        });
    });
};

/** Return a Map of all commits */

export const getCommitsMap = (repo: RepositoryData): Map<string, CommitData> => {
    return new Map(Object.entries(repo.allCommits));
};

/** 
 * Return a list of all branches 
 */

export const getBranches = (repo: RepositoryData): string[] => {
    //get list of branch names 
    return repo.branches.map(branch => branch.branchName);
};


/**
 * Return a list of all contributors
 */
export const getContributors = (repo: RepositoryData): string[] => {
  const contributorsMap = repo.contributors;
  return Array.from(contributorsMap.values()).map(contributor => contributor.name);
};


/**
 * Total commits by contributor (graph-ready)
 * {
 *   title: "All Contributor Commits",
 *   data: [{ name: "Alice", commits: 100 }, ...]
 * }
 */
export function getAllContributorsCommits(data: RepositoryData): {
  title: string;
  data: { name: string; commits: number }[];
} {
  const counts = new Map<string, number>();

  data.allCommits.forEach(commit => {
    const user = commit.contributorName;
    counts.set(user, (counts.get(user) ?? 0) + 1);
  });

  const list = Array.from(counts.entries()).map(([name, commits]) => ({ name, commits }));
  console.log("All contributor commits:", list);
  return { title: "All Contributor Commits", data: list };
}
