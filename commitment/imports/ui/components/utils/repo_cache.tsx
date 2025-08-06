import { Meteor } from 'meteor/meteor'
import { RepositoryData, CommitData, ContributorData, BranchData } from '/server/commitment_api/types';

// Functions to fetch repository data from the RepoCollection in a clean way
// TO DO: add more functions tailoured to MetricPage needs - perhaps a new file ? 

export const getRepoData = async (url: string): Promise<RepositoryData> => {
    return new Promise((resolve, reject)) => {
        Meteor.call('repoCollection.getRepoData', url, (err: Error, result: RepositoryData) => {
            if (err) {
                reject(err);
            } else {
                resolve(result);
            }
        });
    });
};

/** Return a Map of all commits */

export const getCommitsMap = async (url: string): Promise<Map<string, CommitData>> => {
    const repo = await getRepoData(url); 
    return new Map(Object.entries(repo.allCommits));
};

/** 
 * Return a list of all branches 
 */

export const getBranches = async (url: string): Promise<BranchData[]> => {
    const repo = await getRepoData(url); 
    return repo.branches; 
}; 

/**
 * Return a Map of all contributors
 */
export const getContributorsMap = async(url: string): Promise<Map<string, ContributorData>> => {
    const repo = await getRepoData(url); 
    return new Map(Object.entries(repo.contributors));
};