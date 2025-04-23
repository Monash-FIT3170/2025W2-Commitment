// up for change

export type RepositoryData = Readonly<{
    name: string, 
    contributors: ContributorData[],
    branches: BranchData[]
}>;

export type ContributorData = Readonly<{
    name: string
}>; 

export type BranchData = Readonly<{
    branchName: string,
    commits: CommitData[]
}>; 

export type CommitData = Readonly<{
    contributor: ContributorData,
    description: string,
    timestamp: Date,
    fileData: FileData[]
}>; 

export type FileData = Readonly<{
    contents: FileContents,
    changes: ChangeType
}>; 

export type FileContents = Readonly<{
    filename: string,
    filepath: string,
}>;

export type ChangeType = "Create" | Modify | "Destroy"
export type Modify = Readonly<{
    previous: FileContents,
}>; 
