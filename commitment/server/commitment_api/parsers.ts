import { CommandResult, successful } from "./command"
import { last } from "./helpers"
import { 
    ContributorData, 
    FileChanges,
    ChangeData,
    ChangeType,
} from "./types"


export type Maybe<T> = T | null

export const fromMaybe = <T>(m: Maybe<T>, error: string): Promise<T> => m === null ? Promise.reject(new Error(error)) : Promise.resolve(m) 

export const parse = (s: string) => <T>(parser: (text: string) => Maybe<T>): Promise<T> => 
    fromMaybe(parser(s), `parser \"${parser.name}\" failed to parse text:\n${s}`)


// executes p and then captures it for use in multiple parsers (more efficient as it does not have multiple executions of the same promise)
export const parsePromise = async (p: Promise<string>): Promise<<T>(parser: (text: string) => Maybe<T>) => Promise<T>> => {
    const s = await p
    return <T>(parser: (text: string) =>  Maybe<T>): Promise<T> => parse(s)(parser)
} 

// parses and executes p at the same time (less efficient if this is called multiple times but has good syntax for inline statements)
export const parsePromiseImmediate = (p: Promise<string>) => async <T>(parser: (text: string) => Maybe<T>): Promise<T> => 
    parse(await p)(parser)

// prioritises stderror, then error, then a successful message
export const getParsableStringFromCmd = (res: CommandResult): string => 
    (res.stdError != null) ? res.stdError : ((res.error != null) ? res.error.message : res.result)

export const parseCmd = (p: Promise<CommandResult>): Promise<(<T>(parser: (text: string) => Maybe<T>) => Promise<T>)> => 
    parsePromise(p.then(getParsableStringFromCmd))

export const parseCmdImmediate = (p: Promise<CommandResult>): <T>(parser: (text: string) => Maybe<T>) => Promise<T> => 
    parsePromiseImmediate(p.then(getParsableStringFromCmd))

export const parseSuccess = (res: Promise<CommandResult>): Promise<string> => 
    res.then(res => successful(res) ? Promise.reject(new Error(getParsableStringFromCmd(res))) : Promise.resolve(res.result))

export const failedOutput = (text: string): boolean => 
    ["fatal:", "error:", "could not", "not a git repository"]
        .map(text.startsWith)
        .reduce((p, n) => p || n)


export const exactText = (text: string): Maybe<string> => failedOutput(text) ? null : text

export const parseRepoExists = (text: string): Maybe<string> => failedOutput(text) ? null : "repo exists" 

export const parseRepoName = (text: string): Maybe<string> => failedOutput(text) ? null : text.replace("repo-", "")

export const parseRepoBranches = (text: string): Maybe<string[]> => failedOutput(text) ? null : text
    .split("\n") // splits on new line
    .map(line => line.trim().replace(/^\* /, "")) // remove "* " from current branch
    .filter(Boolean); // remove empty lines

export const parseCommitHashes = (text: string): Maybe<string[]> => failedOutput(text) ? null : text
    .split("\n") // splits on new line
    .map(line => line
        .trim()
        .split(" ")[0]
    ) // gets the SHA-8 value for each commit
    .filter(Boolean); // remove empty lines


export const parseContributorData = (text: string): Maybe<ContributorData[]> => failedOutput(text) ? null : text
    .split("\n") // splits on new line
    .map((line) => line
        .trim()    
        .split("|")
    )   
    .map((data) => ({
        name: data[1],
        email: data[2],
        numCommits: Number(data[0]) 
        } as ContributorData)
    )


export const parseCommitData = (text: string): Maybe<Readonly<{
    commitHash: string,
    contributorName: string,
    description: string,
    dateString: string,
    involvedFiles: string[][]
}>> => {
    if (failedOutput(text)) return null

    const lines = text.split("\n")
    const hash = lines[0]
    const authorName = lines[1]
    const dateString = lines[2]
    const description = lines[3]
    const fileLines = [...Array(lines.length - 4)].map(i => lines[i + 4]) // check for bugs

    const data = fileLines.map(line => {
        const frags = line.split(" ", 1)
        const changeClass = frags[0]
        const rest = frags[1].trim()
        // returns the first letter of the change
        return [changeClass[0], rest]
    })

    return {
        commitHash: hash,
        contributorName: authorName,
        description: description,
        dateString: dateString,
        involvedFiles: data
    }
}

export const parseFileDataFromCommit = (getFileContents: (text: string) => Promise<string>, getOldFileContents: (text: string) => Promise<string>) => async (data: string[]): Promise<Maybe<FileChanges>> => {

    const changeType = data[0] as ChangeType
    const rest = data[1].split("|||")
    const newFile = last(rest) as string

    const getChange = async (): Promise<ChangeData> => {
        // if renamed or copied get info for the previous file path
        if (rest.length > 1) {
            // get the old filepath
            const oldFilePath = rest[0]
            // if not a modify then we dont need to grab old file data for a diff
            if (changeType in ["R", "C"]) {
                return {
                    char: changeType,
                    extra: {
                        oldFilePath: oldFilePath
                    }
                }

            } else if (changeType === "M") {
                // grab old file contents
                const oldFileContents = {
                    contents: await getOldFileContents(oldFilePath),
                    filepath: oldFilePath
                }

                return {
                    char: changeType,
                    extra: {
                        previousFile: oldFileContents
                    }
                }

            } else {
                // we should not be here, raise error or smth idk lol
            }
        } 
        // if not just return the change type as no additional information is needed
        return {
            char: changeType,
            extra: null
        }
    }
    
    const updatedFileContents = await getFileContents(newFile)

    const fileContents = {
        contents: updatedFileContents,
        filepath: newFile
    }
    
    return ({
        file: fileContents,
        changes: await getChange()
    } as FileChanges)
}




