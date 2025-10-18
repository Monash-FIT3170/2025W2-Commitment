import { Command, doNotLogData } from "./shell";

export const checkIfExists = (url: string): Command => ({
  ...doNotLogData,
  cmd: `git ls-remote \"${url}\"`,
});

export const cloneToLocal = (url: string, path: string): Command => ({
  ...doNotLogData,
  cmd: `git -c credential.helper= -c core.askPass=true clone --bare \"${url}\" \"${path}\"`,
});

export const getAllBranches = (): Command => ({
  ...doNotLogData,
  cmd: `git --no-pager branch -a --format=\"%(refname:short)\"`,
});

export const getLastCommitDate = (branch: string): Command => ({
  ...doNotLogData,
  // gets the timestamp of the last commit from the branch
  cmd: `git log -1 --format=%cI \"${branch}\"`,
});
