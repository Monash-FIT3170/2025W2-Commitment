import {
  RepositoryData,
  CommitData, 
  FilteredData,
  SerializableRepoData,
  ContributorData
} from "/imports/api/types";

/**
 * Filters repository data based on branch, date range, and optionally contributor.
 * Returns all data in a serializable format.
 */
export const getFilteredRepoDataServer = (
  repo: SerializableRepoData,
  daysBack: number = 7,
  filteredBranchName?: string,
  filteredContributorName?: string
): FilteredData => {
  const end = new Date();
  const start = new Date();
  start.setDate(end.getDate() - daysBack);

  // Find the branch
  const filterBranch = repo.branches.find(b => b.branchName === filteredBranchName);
  const branchCommitHashes = new Set(filterBranch?.commitHashes ?? []);

  // FILTER COMMITS → use the array, not repo.allCommits directly
  const filteredCommits = repo.allCommits.filter(({ key, value }) => {
    const commitDate = new Date(value.timestamp);
    const isInBranchAndDate =
      branchCommitHashes.has(key) &&
      commitDate >= start &&
      commitDate <= end;

    const isCorrectContributor =
      !filteredContributorName || value.contributorName === filteredContributorName;

    return isInBranchAndDate && isCorrectContributor;
  });

  // FILTER CONTRIBUTORS → use the array, not repo.contributors directly
  const filteredContributors = filteredContributorName
    ? repo.contributors.filter(({ key }) => key === filteredContributorName)
    : repo.contributors;

  // RETURN serializable repo
  const filteredRepo: SerializableRepoData = {
    ...repo,
    allCommits: filteredCommits,
    contributors: filteredContributors,
  };

  return {
    dateRange: { start, end },
    repositoryData: filteredRepo,
  };
};