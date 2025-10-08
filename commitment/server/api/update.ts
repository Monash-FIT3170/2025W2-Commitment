import { SerializableRepoData } from "@api/types";
import { join, takeFromBack, compareDates, getLatestCommit } from "/server/helper_functions";
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
  const lastCommitFromDatabase: Date = getLatestCommit(data);

  // works out a relative directory to work with based on the
  // publisher of the repo and the repo name
  const rel_dir = join(takeFromBack(url.split("/"), 2));

  // ensure directory is created
  const temp_working_dir = await createTempDirectory(`/tmp-clone-dir/${rel_dir}`);

  // execute commands in local directory
  const commandLocal = executeCommand(temp_working_dir);

  // checks whether the repository exists
  await commandLocal(checkIfExists(url)).then(assertSuccess("Repository does not exist"));

  // attempts to clone the repository to a local temp directory (that is unique)
  await commandLocal(cloneToLocal(url, temp_working_dir)).then(
    assertSuccess("Failed to clone the repo")
  );

  // gets the date that HEAD was pushed
  const date = await commandLocal(getLastCommitDate()).then(
    assertSuccess("Failed to fetch the HEAD commit details")
  );

  // delete all contents from the temporary directory
  await deleteAllFromDirectory(temp_working_dir);

  // do actual comparison
  const cleanedDate = date.trim();
  const dateObj = new Date(cleanedDate);
  return !compareDates(dateObj, lastCommitFromDatabase);
};

const checkIfExists = (url: string): Command => ({
  ...doNotLogData,
  cmd: `git ls-remote \"${url}\"`,
});

const cloneToLocal = (url: string, path: string): Command => ({
  ...doNotLogData,
  cmd: `git -c credential.helper= -c core.askPass=true clone --bare \"${url}\" \"${path}\"`,
});

const getLastCommitDate = (): Command => ({
  ...doNotLogData,
  cmd: `git log -1 --format=%cI`,
});
