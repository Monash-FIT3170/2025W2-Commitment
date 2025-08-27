import { FilteredData, SerializableRepoData } from "/imports/api/types";

/**
 * Filters repository data based on branch, date range, and optionally contributor.
 * Returns all data in a serializable format.
 *
 * @param repoUrl the URL of the repository.
 * @param start Start date range to filter from.
 * @param end End date range to filter to.
 * @param repo Full repository data.
 * @param filteredBranchName Branch to filter (optional).
 * @param filteredContributorNames Contributors to filter (optional).
 * @returns
 */
export const getFilteredRepoDataServer = (
  repoUrl: string,
  start: Date,
  end: Date,
  repo: SerializableRepoData,
  filteredBranchName: string,
  filteredContributorNames: string[]
): FilteredData => {
  // Find the branch

  console.log("Who are we meant to be filtering out?", filteredContributorNames, typeof filteredContributorNames)
  console.log("Who are we meant to be filtering out?", filteredBranchName, typeof filteredBranchName)
  console.log("Who are we meant to be filtering out?", start, end, typeof start, typeof end)

  const filterBranch = repo.branches.find(
    (b) => b.branchName === filteredBranchName
  );
  const branchCommitHashes = new Set(filterBranch?.commitHashes ?? []);

  // // FILTER COMMITS → use the array, not repo.allCommits directly
  // const filteredCommits = repo.allCommits.filter(({ key, value }) => {
  //   const commitDate = new Date(value.timestamp);
  //   const isInBranchAndDate =
  //     branchCommitHashes.has(key) && commitDate >= start && commitDate <= end;

  //   const isCorrectContributor =
  //     !filteredContributorNames ||
  //     filteredContributorNames.includes(value.contributorName);

  //   return isInBranchAndDate && isCorrectContributor;
  // });

  // // FILTER CONTRIBUTORS
  // const filteredContributors = filteredContributorNames
  //   ? repo.contributors.filter(({ key }) =>
  //       filteredContributorNames.includes(key)
  //     )
  //   : repo.contributors;

  // FILTER COMMITS
  const filteredCommits = repo.allCommits.filter(({ key, value }) => {
    const commitDate = new Date(value.timestamp);
    const isInBranchAndDate =
      branchCommitHashes.has(key) && commitDate >= start && commitDate <= end;

    const isCorrectContributor =
      !filteredContributorNames ||
      filteredContributorNames.includes(value.contributorName);

    return isInBranchAndDate && isCorrectContributor;
  });

  // FILTER CONTRIBUTORS → derive from filteredCommits
  const contributorNames = new Set(
    filteredCommits.map(({ value }) => value.contributorName)
  );

  const filteredContributors = repo.contributors.filter(({ value }) =>
    contributorNames.has(value.name ?? value.name)
  );

  // RETURN serializable repo
  const filteredRepo: SerializableRepoData = {
    ...repo,
    allCommits: filteredCommits.map(({ key, value }) => ({ key, value })),
    contributors: filteredContributors.map(({ key, value }) => ({
      key,
      value,
    })),
  };

  return {
    repoUrl,
    dateRange: { start, end },
    repositoryData: filteredRepo,
  };
};
