// up for change

import { isDate } from "util"

export type RepositoryData = Readonly<{
    name: string
    branches: BranchData[]
    allCommits: Map<string, CommitData>
}>

export type BranchData = Readonly<{
    branchName: string
    commitHashes: string[]
}> 

// make a kind of commit where you have a snapshot of all contributors per line
export type CommitData = Readonly<{
    commitHash: string
    contributor: ContributorData
    description: string
    timestamp: Date
    fileData: FileChanges[]
}> 

export type ContributorData = Readonly<{
    name: string
    email: string
    numCommits: number
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

export type ChangeType = "A" | "M" | "D" | "R" | "C"
// create diff section
export type ModifyData = Readonly<{ previousFile: FileContents }>
export type RenameData = Readonly<{ oldFilePath: string }>
export type CopyData = Readonly<{ oldFilePath: string }>

// typechecking functions
export const isRepositoryData = (o: any): o is RepositoryData => 
    typeof o === "object" 
    && o != null
    && "name"        in o && typeof o.name === "string"
    && "branches"    in o && (!Array.isArray(o.branches)) && o.branches.every(isBranchData)

export const isBranchData = (o: any): o is BranchData => 
    typeof o === "object" 
    && o != null
    && "branchName"  in o && typeof o.branchName === "string"
    && "commits"     in o && (!Array.isArray(o.commits)) && o.commits.every(isCommitData)

export const isCommitData = (o: any): o is CommitData => 
    typeof o === "object" 
    && o != null
    && "commitHash"  in o && typeof o.commitHash === "string"
    && "contributor" in o && isContributorData(o.contributor)
    && "description" in o && typeof o.description === "string"
    && "timestamp"   in o && isDate(o.timestamp) // may need to change as function is depricated
    && "fileData"    in o && (!Array.isArray(o.fileData)) && o.fileData.every(isFileChanges)

export const isContributorData = (o: any): o is ContributorData => 
    typeof o === "object" 
    && o != null
    && "name"        in o && typeof o.name === "string"
    && "email"       in o && typeof o.email === "string"
    && "numCommits"  in o && typeof o.numCommits === "number"

export const isFileChanges = (o: any): o is FileChanges => 
    typeof o === "object" 
    && o != null
    && "file"        in o && isFileContents(o.file)
    && "changes"     in o && isChangeType(o.changes)

export const isFileContents = (o: any): o is FileContents => 
    typeof o === "object" 
    && o != null
    && "contents"    in o && typeof o.contents === "string"
    && "filepath"    in o && typeof o.filepath === "string"
    
export const isChangeType = (o: any): o is ChangeType => 
    o === 'Added' || o === 'Deleted'
    || 
    typeof o === "object" 
    && o != null
    && (     
        ("previousFile"     in o && isFileContents(o.file))
        ||  
        ("previousFilePath" in o && typeof o.previousFile === "string")
    )


export const getTypeBy = (c: string): ChangeType | null => {
    switch (c) {
        case "A" : 
        case "D" : 
        case "M" : 
        case "R" :
        case "C" : return c
        default  : return null
    }
}

export const sortCommitByTimeStamp = (c1: CommitData, c2: CommitData): number => c2.timestamp.getTime() - c1.timestamp.getTime()
