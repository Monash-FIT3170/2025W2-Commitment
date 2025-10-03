import { CommitData, SerializableRepoData } from "@api/types";
import { AliasConfig, StudentAlias } from "@api/alias_configs";
import { zip, getAllContributorsCommitCounts } from "./helper_functions"

// Helper function to get commit counts for all contributors

export function getAllStudentCommits(data: SerializableRepoData, config: AliasConfig) {
  const res = getAllContributorsCommitCounts(data);
  return zip(config.aliases, config.aliases.map((a: StudentAlias) => {
    const aliases = a.gitUsernames;
    return res
      .filter((d: { name: string; value: number }) => aliases.includes(d.name))
      .map((v: { name: string; value: number }) => v.value)
      .reduce((acc: number, i: number) => acc + i, 0);
  }));
}

