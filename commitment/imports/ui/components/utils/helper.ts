import { RepositoryData, CommitData, ContributorData, BranchData } from '/server/commitment_api/types';


export interface SerializableRepoData {
    name: string; 
    branches: BranchData[]; 
    allCommits: {key:string; value: CommitData}[]; // Map converted to a list of objects
    contributors: {key:string; value: ContributorData}[]; // Map converted to a list of objects
}

/**
 * Convert serialized repo data (plain objects) back into RepositoryData with Maps.
 */
export function deserializeRepoData(data: SerializableRepoData): RepositoryData {
    return {
        ...data,
        allCommits: new Map((data.allCommits ).map(entry => [entry.key, entry.value])),
        contributors: new Map((data.contributors ).map(entry => [entry.key, entry.value])),
    };
}