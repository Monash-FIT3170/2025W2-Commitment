// up for change

import { isDate } from 'util';


/**
 * (DE)SERIALIZABLE TYPES 
 * 
 * Types used for serializing and deserializing data between the server and client.
 */

export interface SerializableRepoData {
    name: string; 
    branches: BranchData[]; 
    allCommits: {key:string; value: CommitData}[]; // Map converted to a list of objects
    contributors: {key:string; value: ContributorData}[]; // Map converted to a list of objects
}


export interface ServerRepoData {
  _id?: string
  url: string
  createdAt: Date
  data: RepositoryData
}


/**
 * REPOSITORY DATA TYPES
 * 
 * Types representing the structure of a repository, including branches, commits, contributors, and file changes.
 */

export type RepositoryData = Readonly<{
    name: string
    branches: BranchData[]
    allCommits: Map<string, CommitData>
    contributors: Map<string, ContributorData>
}>

export type BranchData = Readonly<{
    branchName: string
    commitHashes: string[]
}>

// make a kind of commit where you have a snapshot of all contributors per line
export type CommitData = Readonly<{
    commitHash: string
    commitTitle: string
    contributorName: string
    description: string
    timestamp: Date
    fileData: FileChanges[]
}>

export type ContributorData = Readonly<{
    name: string
    emails: string[]
}>

export type FileChanges = Readonly<{
    file: FileContents
    changes: ChangeData
}>

export type FileContents = Readonly<{
    contents: string
    filepath: string
}>

export type ChangeData = Readonly<{
    char: ChangeType,
    extra: null | ModifyData | RenameData | CopyData
}>

export type ChangeType = 'A' | 'M' | 'D' | 'R' | 'C'
// create diff section and greatly improve efficiency of storage by hashing FileContents so no repeated information is stashed in the DB
export type ModifyData = Readonly<{ previousFile: FileContents }>
export type RenameData = Readonly<{ oldFilePath: string, likeness: number }>
export type CopyData = Readonly<{ oldFilePath: string, likeness: number }>

const isMapOf = <K, V>(
  value: unknown,
  isKey: (k: unknown) => k is K,
  isValue: (v: unknown) => v is V,
): value is Map<K, V> => value instanceof Map
    && Array.from(value.entries()).every(([k, v]) => isKey(k) && isValue(v));

// typechecking functions
export const isRepositoryData = (o: any): o is RepositoryData => typeof o === 'object'
    && o != null
    && 'name' in o && typeof o.name === 'string'
    && 'branches' in o && (Array.isArray(o.branches)) && o.branches.every(isBranchData)
    && 'allCommits' in o && isMapOf(o.allCommits, (s) => typeof s === 'string', isCommitData);

export const isBranchData = (o: any): o is BranchData => typeof o === 'object'
    && o != null
    && 'branchName' in o && typeof o.branchName === 'string'
    && 'commitHashes' in o && (!Array.isArray(o.commits)) && o.commits.every(isCommitData);

export const isCommitData = (o: any): o is CommitData => typeof o === 'object'
    && o != null
    && 'commitHash' in o && typeof o.commitHash === 'string'
    && 'contributor' in o && isContributorData(o.contributor)
    && 'description' in o && typeof o.description === 'string'
    && 'timestamp' in o && isDate(o.timestamp) // may need to change as function is depricated
    && 'fileData' in o && (!Array.isArray(o.fileData)) && o.fileData.every(isFileChanges);

export const isContributorData = (o: any): o is ContributorData => typeof o === 'object'
    && o != null
    && 'name' in o && typeof o.name === 'string'
    && 'email' in o && typeof o.email === 'string';

export const isFileChanges = (o: any): o is FileChanges => typeof o === 'object'
    && o != null
    && 'file' in o && isFileContents(o.file)
    && 'changes' in o && isChangeType(o.changes);

export const isFileContents = (o: any): o is FileContents => typeof o === 'object'
    && o != null
    && 'contents' in o && typeof o.contents === 'string'
    && 'filepath' in o && typeof o.filepath === 'string';

export const isChangeType = (o: any): o is ChangeType => o === 'Added' || o === 'Deleted'
    || typeof o === 'object'
    && o != null
    && (
      ('previousFile' in o && isFileContents(o.file))
        || ('previousFilePath' in o && typeof o.previousFile === 'string')
    );

export const getTypeBy = (c: string): ChangeType | null => {
  switch (c) {
    case 'A':
    case 'D':
    case 'M':
    case 'R':
    case 'C': return c;
    default: return null;
  }
};

export const sortCommitByTimeStamp = (c1: CommitData, c2: CommitData): number => c2.timestamp.getTime() - c1.timestamp.getTime();
