import { SerializableRepoData } from "/imports/api/types";
import { CommitData } from "/imports/api/types";
import { getAllCommits } from "/server/helper_functions";
import {
  Command,
  assertSuccess,
  doNotLogData,
  executeCommand,
  createTempDirectory,
  deleteAllFromDirectory,
} from "./shell";

const compareDates = (d1: Date, d2: Date): boolean =>
  d1.valueOf() > d2.valueOf();

const takeFromBack = <T>(arr: T[], num: number): T[] => arr.slice(-num);

const join = (arr: string[]): string => arr.reduce((acc, i) => acc + i, "");

/**
 * checks whether a repository splat is up to date with the real version on github (doesn't need to clone anything to the server, can just use remote query)
 * @param data the data to check whether it is up to date or not
 * @returns whether the data is up to date
 */
export const isUpToDate = async (
  url: string,
  data: SerializableRepoData
): Promise<boolean> => {
  const lastCommitFromDatabase: Date = getAllCommits(data).reduce(
    (acc: Date, c: CommitData) =>
      compareDates(acc, c.timestamp) ? acc : c.timestamp,
    new Date(0) // git didn't exist here so its fine :D
  );

  const rel_dir = join(takeFromBack(url.split("/"), 2));
  // ensure directory is created
  const temp_working_dir = await createTempDirectory(
    `/tmp-clone-dir/${rel_dir}`
  );

  // execute commands in local directory
  const commandLocal = executeCommand(temp_working_dir);
  const hash = await commandLocal(getLatestCommit(url)).then(
    assertSuccess(`Failed to fetch HEAD from ${url}`)
  );
  await commandLocal(fetchFromHEAD(url, hash)).then(
    assertSuccess("Failed to clone the repo")
  );
  const date = await commandLocal(getDateFrom(hash)).then(
    assertSuccess("Failed to fetch the HEAD commit details")
  );

  // delete all contents from the temporary directory
  await deleteAllFromDirectory(temp_working_dir);

  // do actual comparison
  const dateObj = new Date(date);
  return !compareDates(dateObj, lastCommitFromDatabase);
};

const getLatestCommit = (url: string): Command => ({
  ...doNotLogData,
  cmd: `git ls-remote \"${url}\" HEAD`,
});

const fetchFromHEAD = (url: string, hash: string): Command => ({
  ...doNotLogData,
  cmd: `git fetch --quiet \"${url}\" ${hash}`,
});

const getDateFrom = (hash: string): Command => ({
  ...doNotLogData,
  cmd: `git show -s --format=%ci ${hash}`,
});
