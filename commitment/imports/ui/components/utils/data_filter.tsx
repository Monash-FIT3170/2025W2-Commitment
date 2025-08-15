

// ------------ IMPORTS ---------------------------------------------------
import { RepositoryData, CommitData, SerializableRepoData} from "/server/commitment_api/types";
import { deserializeRepoData } from '/imports/api/deserialise';
import { Subject } from "rxjs";


// ------------ METHOD TO GET THE REPO DATA BASED OFF A URL ---------------
/** 
 * Fetches the repository data from the server.
 * @param url The url of the repository
 * @param notifier A Subject to notify about the fetching process
 * @returns A promise that resolves to the repository data
 */
export const getRepoData = async (url: string, notifier: Subject<string>): Promise<RepositoryData> => {
    console.log("this method called with url:", url);
    return new Promise((resolve, reject) => {
        notifier.next('Fetching repo data...');
        // this is the NEW ERROR FOR NOW because i couldn't figure out how to transition to the metricspage correctly 
        // Meteor.call(repoCollection.getData) is what should be called. 
        Meteor.call('repoCollection.getData', url, (err: Error, result: SerializableRepoData) => {
            console.log("in getrepodata")
            if (err) {
                console.log(`Error fetching repo data for URL ${url}:`, err);
                notifier.next(`Error fetching repo data: ${err.message}`);
                reject(err);
            } else {
                notifier.next('Repo data fetched successfully.');
                console.log("In metric functions:", result);
                console.log(typeof result.allCommits)
                console.log(result.allCommits)

                const dRepo = deserializeRepoData(result);

                resolve(dRepo);
            }
        });
    });
};

// ------------ METHODS TO FILTER THE REPO DATA -------------------------------- 
// storing Date Range, RepositoryData in a data structure
export interface FilteredData {
    dateRange: {
        start: Date;
        end: Date;
    };
    repositoryData: Promise<RepositoryData>;
}

/**
 * This method filters the repository data based on the provided parameters.
 * @param url The repo url
 * @param notifier 
 * @param daysBack Defines the date range for the repository data, defaulting to 7 days back. 
 * @param filteredBranchName Defines the specific branch name in use , defaulting to the main branch. 
 * @returns 
 */
export const getFilteredRepoData = (
    url: string,
    notifier: Subject<string>,
    daysBack: number = 7,
    filteredBranchName: string = "main",
    filteredContributorName?: string // optional
): FilteredData => {
    const end = new Date();
    const start = new Date();
    start.setDate(end.getDate() - daysBack);

    const repositoryData = getRepoData(url, notifier).then((repo) => {
        // filters to the branch provided 
        const filterBranch = repo.branches.find(b => b.branchName === filteredBranchName);
        if (!filterBranch) {
            console.warn(`Branch ${filteredBranchName} not found in repo.`);
            return { ...repo, allCommits: new Map(), contributors: repo.contributors };
        }

        const branchCommitHashes = new Set(filterBranch.commitHashes);

        // Filter commits by branch, date, and optionally contributor
        const filteredCommits = new Map<string, CommitData>(
            Array.from(repo.allCommits.entries()).filter(([hash, commit]) => {
                const commitDate = new Date(commit.timestamp);
                const isInBranchAndDate = branchCommitHashes.has(hash) &&
                                          commitDate >= start &&
                                          commitDate <= end;
                const isCorrectContributor = !filteredContributorName || commit.contributorName === filteredContributorName;

                return isInBranchAndDate && isCorrectContributor;
            })
        );
        // Filter contributors map if a filteredContributorName is provided
        const filteredContributors = filteredContributorName
            ? new Map(
                Array.from(repo.contributors.entries())
                     .filter(([name]) => name === filteredContributorName)
              )
            : repo.contributors; // keep all if no filter

        return {
            ...repo,
            allCommits: filteredCommits,
            contributors: filteredContributors
        };
    });

    return {
        dateRange: { start, end },
        repositoryData
    };
};

