
import { RepositoryData, CommitData } from "./commitment_api/types";

import { AliasConfig, StudentAlias } from "..imports/api/alias_config"
import { number } from "zod";

import {
    getAllContributorsCommits
} from "./repo_metrics"

export const zip = <T extends any[][]>(...lists: T): { [K in keyof T]: T[K][number] }[] => Array.from({ length: Math.min(...lists.map((list) => list.length)) }, (_, i) => lists.map((list) => list[i]) as { [K in keyof T]: T[K][number] });


export function getAllStudentCommits(data: RepositoryData, config: AliasConfig) {
  const res = getAllContributorsCommits(data).data
  return zip(config.aliases, config.aliases.map((a: StudentAlias) => {
    const aliases = a.gitUsernames
    return res
      .filter(d => aliases.contains(d.name))
      .map((v => v.commits))
      .reduce((acc: number, i: number) => acc + i, 0)
  }))
} 
