export type RepositoryData = Readonly<{
  name: string;
  branches: BranchData[];
  allCommits: Map<string, CommitData>;
  contributors: Map<string, ContributorData>;
}>;

export type SerialisableMapObject<K, V> = {
  key: K;
  value: V;
};

export type SerializableRepoData = Readonly<{
  name: string;
  branches: BranchData[];
  allCommits: SerialisableMapObject<string, CommitData>[]; // Map converted to a list of objects
  contributors: SerialisableMapObject<string, ContributorData>[]; // Map converted to a list of objects
}>;
export interface FilteredData {
  dateRange: {
    start: Date;
    end: Date;
  };
  repositoryData: SerializableRepoData;
}

export type BranchData = Readonly<{
  branchName: string;
  commitHashes: string[];
}>;

// make a kind of commit where you have a snapshot of all contributors per line
export type CommitData = Readonly<{
  commitHash: string;
  commitTitle: string;
  contributorName: string;
  description: string;
  timestamp: Date;
  fileData: FileChanges[];
}>;

export type ContributorData = Readonly<{
  name: string;
  emails: string[];
}>;

export type FileChanges = Readonly<{
  filepath: string    
  oldFilePath: string 
  char: ChangeType        
  likeness: number    
  newLines: number    
  deletedLines: number
  diff: [string]        
}>;

export type ChangeType = "A" | "M" | "D" | "R" | "C";

export type AliasEmail = {
  username: string;
  email: string;
};

export type UserScalingSummary = {
  name: string;
  aliases: AliasEmail[];
  finalGrade: number | null; // may be missing -> if there is a grading sheet, this must be populated
  scale: number;  // always present
};

export type ContributionEntry = {
  name: string;
  date: string;
  count: number;
};
