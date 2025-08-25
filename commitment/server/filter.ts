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
 * @param repo Full repository data
 * @param daysBack Number of days to look back (default: 7)
 * @param filteredBranchName Branch to filter (optional)
 * @param filteredContributorName Contributor to filter (optional)
 */
export const getFilteredRepoDataServer = (
  start: Date,
  end: Date,
  repo: SerializableRepoData,
  filteredBranchName?: string,
  filteredContributorName?: string[],
): FilteredData => {

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

  // FILTER CONTRIBUTORS 
  const filteredContributors = filteredContributorName
    ? repo.contributors.filter(({ key }) => key === filteredContributorName)
    : repo.contributors;

  // RETURN serializable repo
  const filteredRepo: SerializableRepoData = {
    ...repo,
    allCommits: filteredCommits.map(({ key, value }) => ({ key, value })),
    contributors: filteredContributors.map(({ key, value }) => ({ key, value })),
  };

  console.log("Filtered Repo Data in FILTER FUNC!!!:", filteredRepo);

  return {
    dateRange: { start, end },
    repositoryData: filteredRepo,
  };
};