import { Meteor } from 'meteor/meteor'
import { RepositoryData, CommitData, ContributorData, BranchData } from '/server/commitment_api/types';
import { Subject } from "rxjs";
import { SerializableRepoData, deserializeRepoData } from './helper';
// import { StringDecoder } from 'string_decoder';


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
