import {
  SerialisableMapObject,
  SerializableRepoData,
  RepositoryData,
  BranchData,
  CommitData,
  ContributorData,
  FileChanges,
  ChangeType,
} from "/imports/api/types";

/**
 * Convert RepositoryData's Maps into plain objects to store in DB.
 */
export const mapToArray = <K, V>(m: unknown): SerialisableMapObject<K, V>[] => {
  if (m instanceof Map) {
    return Array.from(m.entries()).map(([key, value]) => ({ key, value }));
  } else if (m && typeof m === "object") {
    // plain object fallback
    return Object.entries(m).map(([key, value]) => ({
      key: key as K,
      value: value as V,
    }));
  }
  return [];
};

const arrayToMap = <K, V>(a: unknown): Map<K, V> => {
  if (Array.isArray(a)) {
    // Convert array of objects to array of [key, value] tuples
    const entries: [K, V][] = a.map((e) => {
      const obj = e as SerialisableMapObject<K, V>;
      return [obj.key, obj.value];
    });
    return new Map(entries);
  } else if (a && typeof a === "object") {
    return objectToMap(a) as Map<K, V>;
  }
  return new Map();
};

const objectToMap = <K, V>(a: unknown): Map<K, V> | null =>
  a && typeof a === "object"
    ? new Map(Object.entries(a).map(([key, value]) => [key as unknown as K, value as V]))
    : null;

export const serializeRepoData = (data: RepositoryData): SerializableRepoData => ({
  ...data,
  allCommits: mapToArray<string, CommitData>(data.allCommits),
  contributors: mapToArray<string, ContributorData>(data.contributors),
});

export const deserializeRepoData = (data: SerializableRepoData): RepositoryData => ({
  ...data,
  allCommits: arrayToMap<string, CommitData>(data.allCommits),
  contributors: arrayToMap<string, ContributorData>(data.contributors),
});

type assertType<T> = T;

export const assertRepoTyping = (data: RepositoryData): RepositoryData =>
  ({
    name: assertTypeOf<string>(data.name),
    branches: assertArrayOf(data.branches, assertBranchData),
    allCommits: assertMapOf(data.allCommits, assertCommitData),
    contributors: assertMapOf(data.contributors, assertContributorData),
  } as RepositoryData);

const assertArrayOf = <T>(arr: T[], typecheckf: (t: assertType<T>) => T): T[] =>
  arr.map(typecheckf);

const assertMapOf = <T>(m: Map<string, T>, typecheckf: (t: assertType<T>) => T): Map<string, T> => {
  if (m instanceof Map) {
    return new Map(Array.from(m, ([k, v]) => [k, typecheckf(v)] as [string, T]));
  } else {
    return objectToMap(m)!;
  }
};

const assertTypeOf = <T>(t: assertType<T>): T => t as T;

const assertBranchData = (b: BranchData): BranchData =>
  ({
    branchName: b.branchName as string,
    commitHashes: assertArrayOf(b.commitHashes, assertTypeOf<string>),
  } as BranchData);

const assertCommitData = (c: CommitData): CommitData =>
  ({
    commitHash: assertTypeOf<string>(c.commitHash),
    commitTitle: assertTypeOf<string>(c.commitTitle),
    contributorName: assertTypeOf<string>(c.contributorName),
    description: assertTypeOf<string>(c.description),
    timestamp: assertTypeOf<Date>(c.timestamp),
    fileData: assertArrayOf(c.fileData, assertFileChanges),
  } as CommitData);

const assertFileChanges = (f: FileChanges): FileChanges =>
  ({
    filepath: assertTypeOf<string>(f.filepath),
    oldFilePath: assertTypeOf<string>(f.oldFilePath),
    char: assertTypeOf<ChangeType>(f.char),
    likeness: assertTypeOf<number>(f.likeness),
    newLines: assertTypeOf<number>(f.newLines),
    deletedLines: assertTypeOf<number>(f.deletedLines),
    diff: assertArrayOf(f.diff, assertTypeOf<string>),
  } as FileChanges);

const assertContributorData = (c: ContributorData): ContributorData =>
  ({
    name: assertTypeOf<string>(c.name),
    emails: assertArrayOf(c.emails, assertTypeOf<string>),
  } as ContributorData);
