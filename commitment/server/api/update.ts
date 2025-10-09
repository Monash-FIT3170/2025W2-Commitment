import { SerializableRepoData } from "@api/types";
import {
  join,
  takeFromBack,
  compareDates,
  getLatestCommit,
  getLatestDate,
  getAllCommits,
} from "/server/helper_functions";
import {
  Command,
  assertSuccess,
  doNotLogData,
  executeCommand,
  createTempDirectory,
  deleteAllFromDirectory,
} from "./shell";

/**
 * checks whether a repository splat is up to date with the real version on github (doesn't need to clone anything to the server, can just use remote query)
 * @param data the data to check whether it is up to date or not
 * @returns whether the data is up to date
 */
export const isUpToDate = async (url: string, data: SerializableRepoData): Promise<boolean> => {
  // works out a relative directory to work with based on the
  // publisher of the repo and the repo name
  const rel_dir = join(takeFromBack(url.split("/"), 2));

  // ensure directory is created
  const temp_working_dir = await createTempDirectory(`/tmp-clone-dir/${rel_dir}`);

  try {
    const latestCommit = getLatestCommit(getAllCommits(data));
    if (latestCommit === null) return false;
    const lastDate: Date = new Date(latestCommit.timestamp);

    // execute commands in local directory
    const commandLocal = executeCommand(temp_working_dir);

    // checks whether the repository exists
    await commandLocal(checkIfExists(url)).then(assertSuccess("Repository does not exist"));

    // attempts to clone the repository to a local temp directory (that is unique)
    await commandLocal(cloneToLocal(url, temp_working_dir)).then(
      assertSuccess("Failed to clone the repo")
    );

    const foundBranches = await commandLocal(getAllBranches())
      .then(assertSuccess("Failed to fetch branches"))
      .then((s: string) =>
        s
          .split("\n")
          .map((s: string) => s.trim())
          .filter((s: string) => s !== "")
      );

    const dates = await Promise.all(
      foundBranches.map((branch: string) =>
        commandLocal(getLastCommitDate(branch))
          .then(assertSuccess(`Failed to fetch head commit from branch: ${branch}`))
          .then((d: string) => new Date(d.trim()))
      )
    );
    const mostRecentDate = getLatestDate(dates);

    // do actual comparison
    if (mostRecentDate === null) throw Error("dates is empty");
    return !compareDates(mostRecentDate, lastDate);
  } finally {
    // always delete all contents from the temporary directory
    await deleteAllFromDirectory(temp_working_dir);
  }
};

const checkIfExists = (url: string): Command => ({
  ...doNotLogData,
  cmd: `git ls-remote \"${url}\"`,
});

const cloneToLocal = (url: string, path: string): Command => ({
  ...doNotLogData,
  cmd: `git -c credential.helper= -c core.askPass=true clone --bare \"${url}\" \"${path}\"`,
});

const getAllBranches = (): Command => ({
  ...doNotLogData,
  cmd: `git --no-pager branch -a --format=\"%(refname:short)\"`,
});

const getLastCommitDate = (branch: string): Command => ({
  ...doNotLogData,
  // gets the timestamp of the last commit from the branch
  cmd: `git log -1 --format=%cI \"${branch}\"`,
});
