import { Meteor } from 'meteor/meteor'
import { RepositoryData, CommitData, ContributorData, BranchData } from '/server/commitment_api/types';
import { Subject } from "rxjs";
import { SerializableRepoData, deserializeRepoData } from './helper';
// import { StringDecoder } from 'string_decoder';

// Functions to fetch repository data from the RepoCollection in a clean way
// TO DO: add more functions tailoured to MetricPage needs - perhaps a new file ? 

// this function is not being called by the metrics page correctly...
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

export const getBranches = (repo: RepositoryData): BranchData[] => {
    return repo.branches; 
}; 

/**
 * Return a Map of all contributors
 */
export const getContributorsMap = (repo: RepositoryData): Map<string, ContributorData> => {
    return new Map(Object.entries(repo.contributors));
};

/**
 * Return a list of all contributors
 */
export const getContributors = (repo: RepositoryData): string[] => {
  const contributorsMap = getContributorsMap(repo);
  return Array.from(contributorsMap.values()).map(contributor => contributor.name);};
