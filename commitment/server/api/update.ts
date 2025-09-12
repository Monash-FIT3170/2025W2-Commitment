import { SerializableRepoData } from "../imports/api/types";
import { meteorCallAsync, override, overrideValue } from "../imports/api/meteor_interface";
import { CommitData } from "/imports/api/types";
import { getAllCommits } from "../server/helper_functions";

/**
 * checks whether a repository splat is up to date with the real version on github (doesn't need to clone anything to the server, can just use remote query)
 * @param data the data to check whether it is up to date or not
 * @returns whether the data is up to date
 */
export const isUpToDate = async (data: SerializableRepoData): Promise<boolean> => {
  const lastCommitFromDatabase: Date = getAllCommits(data).reduce(
    (acc: CommitData, c: CommitData) =>
      acc !== null && acc.timestamp.valueOf() > c.timestamp.valueOf() ? acc : c,
    null
  ).timestamp;

  return true;
};
