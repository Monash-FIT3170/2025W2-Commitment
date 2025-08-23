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
  repo: SerializableRepoData,
  daysBack: number = 7,
  filteredBranchName?: string,
  filteredContributorName?: string
): FilteredData => {
  const end = new Date();
  const start = new Date();
  start.setDate(end.getDate() - daysBack);

  // Convert plain objects into arrays of { key, value } if not already arrays
  const allCommitsArray: { key: string; value: CommitData }[] = Array.isArray(repo.allCommits)
    ? repo.allCommits as { key: string; value: CommitData }[]
    : Object.entries(repo.allCommits as Record<string, CommitData>).map(([key, value]) => ({ key, value }));

  const contributorsArray: { key: string; value: ContributorData }[] = Array.isArray(repo.contributors)
    ? repo.contributors as { key: string; value: ContributorData }[]
    : Object.entries(repo.contributors as Record<string, ContributorData>).map(([key, value]) => ({ key, value }));

  // Find the branch
  const filterBranch = repo.branches.find(b => b.branchName === filteredBranchName);
  const branchCommitHashes = new Set(filterBranch?.commitHashes ?? []);

  // FILTER COMMITS 
  const filteredCommits = allCommitsArray.filter(({ key, value }) => {
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
    ? contributorsArray.filter(({ key }) => key === filteredContributorName)
    : contributorsArray;

  // RETURN serializable repo
  const serializableRepo: SerializableRepoData = {
    ...repo,
    allCommits: filteredCommits.map(({ key, value }) => ({ key, value })),
    contributors: filteredContributors.map(({ key, value }) => ({ key, value })),
  };

  return {
    dateRange: { start, end },
    repositoryData: serializableRepo,
  };
};