import {
  RepositoryData,
  CommitData, 
  FilteredData
} from "/imports/api/types";



// ------------ METHODS TO FILTER THE REPO DATA --------------------------------
// storing Date Range, RepositoryData in a data structure

/**
 * Filters repository data based on branch, date range, and optionally contributor.
 * @param repo The complete repository data (from cache or DB)
 * @param daysBack Number of days to look back for commits (default: 7)
 * @param filteredBranchName Name of the branch to filter on (default: "main")
 * @param filteredContributorName (Optional) Specific contributor to filter on
 * @returns FilteredData object
 */
export const getFilteredRepoDataServer = (
  repo: RepositoryData,
  daysBack: number = 7,
  filteredBranchName?: string,
  filteredContributorName?: string
): FilteredData => {
  const end = new Date();
  const start = new Date();
  start.setDate(end.getDate() - daysBack);

  // Find the specified branch
  const filterBranch = repo.branches.find(
    (b) => b.branchName === filteredBranchName
  );

  if (!filterBranch) {
    console.warn(`Branch ${filteredBranchName} not found in repo.`);
    return {
      dateRange: { start, end },
      repositoryData: {
        ...repo,
        allCommits: new Map(),
        contributors: repo.contributors,
      },
    };
  }

  const branchCommitHashes = new Set(filterBranch.commitHashes);

  // Filter commits by branch, date, and optionally contributor
  const filteredCommits = new Map<string, CommitData>(
    Array.from(repo.allCommits.entries()).filter(([hash, commit]) => {
      const commitDate = new Date(commit.timestamp);
      const isInBranchAndDate =
        branchCommitHashes.has(hash) &&
        commitDate >= start &&
        commitDate <= end;
      const isCorrectContributor =
        !filteredContributorName ||
        commit.contributorName === filteredContributorName;

      return isInBranchAndDate && isCorrectContributor;
    })
  );

  // Filter contributors if contributor name is provided
  const filteredContributors = filteredContributorName
    ? new Map(
        Array.from(repo.contributors.entries()).filter(
          ([name]) => name === filteredContributorName
        )
      )
    : repo.contributors;

  return {
    dateRange: { start, end },
    repositoryData: {
      ...repo,
      allCommits: filteredCommits,
      contributors: filteredContributors,
    },
  };
};



