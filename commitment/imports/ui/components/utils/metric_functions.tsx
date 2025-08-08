import { Meteor } from 'meteor/meteor'
import { RepositoryData, CommitData, ContributorData, BranchData } from '/server/commitment_api/types';
// import { StringDecoder } from 'string_decoder';

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
    return Array.from(getContributorsMap(repo).values())
        .map(contributor => contributor.name);
};
