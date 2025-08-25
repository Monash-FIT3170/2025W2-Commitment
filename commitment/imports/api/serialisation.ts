
import {
    SerialisableMapObject,
    RepositoryData,
    SerializableRepoData,
    CommitData,
    ContributorData
} from "/imports/api/types"

/**
 * Convert RepositoryData's Maps into plain objects to store in DB.
 */

export const mapToArray = <K, V>(m: unknown): SerialisableMapObject<K, V>[] => {
  if (m instanceof Map) {
    return Array.from(m.entries()).map(([key, value]) => ({ key, value }))
  } else if (m && typeof m === "object") {
    // plain object fallback
    return Object.entries(m).map(([key, value]) => ({
      key: key as K,
      value: value as V,
    }))
  }
  return []
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
    return new Map(
      Object.entries(a).map(([key, value]) => [key as unknown as K, value as V])
    );
  }
  return new Map();
};

export function serializeRepoData(data: RepositoryData): SerializableRepoData {
  return {
    ...data,
    allCommits: mapToArray<string, CommitData>(data.allCommits),
    contributors: mapToArray<string, ContributorData>(data.contributors),
  }
}

export function deserializeRepoData(data: SerializableRepoData): RepositoryData {
  return {
    ...data,
    allCommits: arrayToMap<string, CommitData>(data.allCommits),
    contributors: arrayToMap<string, ContributorData>(data.contributors),
  }
}