import { RepositoryData, SerializableRepoData } from "/server/commitment_api/types";

/**
 * Convert serialized repo data (plain objects) back into RepositoryData with Maps.
 * 
 * @param data Serialisable Repository Data object that is read from database.
 * @returns a ReositoryData object with allCommits and contributors as Maps.
 */
export function deserializeRepoData(data: SerializableRepoData): RepositoryData {
    return {
        ...data,
        allCommits: new Map((data.allCommits ).map(entry => [entry.key, entry.value])),
        contributors: new Map((data.contributors ).map(entry => [entry.key, entry.value])),
    };
}
