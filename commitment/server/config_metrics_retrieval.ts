
import { SerializableRepoData } from "../imports/api/types";
import { AliasConfig, StudentAlias } from "../imports/api/alias_configs";

export const zip = <T extends any[][]>(...lists: T): { [K in keyof T]: T[K][number] }[] => Array.from({ length: Math.min(...lists.map((list) => list.length)) }, (_, i) => lists.map((list) => list[i]) as { [K in keyof T]: T[K][number] });


// Helper function to get commit counts for all contributors
function getAllContributorsCommitCounts(data: SerializableRepoData): { name: string; value: number }[] {
  const counts: Record<string, number> = {};
  
  data.allCommits.forEach(commit => {
    const contributor = commit.value.contributorName;
    counts[contributor] = (counts[contributor] || 0) + 1;
  });
  
  return Object.entries(counts).map(([name, value]) => ({ name, value }));
}

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

// ---- Extra helpers for config-file feature needs ----

// Count "LOC changed" for one commit (Serializable shape)
function getLOCFromSerializableCommit(commit: SerializableRepoData["allCommits"][number]): number {
  const files = commit.value.fileData ?? [];
  return files.reduce((acc, fileChange) => {
    const contents = fileChange.file?.contents ?? "";
    return acc + contents.split("\n").length;
  }, 0);
}

/** Total LOC by contributor (graph-ready: [{ name, value }]) */
export function getTotalLocDataSerializable(
  data: SerializableRepoData
): { name: string; value: number }[] {
  const locs = new Map<string, number>();
  data.allCommits.forEach((commit) => {
    const user = commit.value.contributorName;
    const loc = getLOCFromSerializableCommit(commit);
    locs.set(user, (locs.get(user) ?? 0) + loc);
  });
  return Array.from(locs.entries()).map(([name, value]) => ({ name, value }));
} 
