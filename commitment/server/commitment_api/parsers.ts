import { CommandResult, successful } from "./command"
import { last } from "./helpers"
import { 
    FileChanges,
    ChangeData,
    ChangeType,
} from "./types"


export type Maybe<T> = T | null

export const fromMaybe = <T>(m: Maybe<T>, error: string): T =>
  m === null ? (() => { throw new Error(error); })() : m;

export const parse = (s: string) => <T>(parser: (text: string) => Maybe<T>): T => 
    fromMaybe(parser(s), `parser \"${parser.name}\" failed to parse text:\n${s}`)

// prioritises stderror, then error, then a successful message
export const getParsableStringFromCmd = (res: CommandResult): string => 
    (res.stdError != null) ? res.stdError : ((res.error != null) ? res.error.message : res.result)

export const parseCmd = (p: CommandResult): (<T>(parser: (text: string) => Maybe<T>) => T) => 
    parse(getParsableStringFromCmd(p))

export const assertSuccess = (res: CommandResult) => 
    successful(res) ? (() => { throw new Error(getParsableStringFromCmd(res)); })() : null

export const failedOutput = (text: string): boolean => 
    ["fatal:", "error:", "could not", "not a git repository"]
        .map(s => text.startsWith(s))
        .reduce((p, n) => p || n)

export const exactText = (text: string): Maybe<string> => failedOutput(text) ? null : text

export const parseRepoExists = (text: string): Maybe<string> => failedOutput(text) ? null : "repo exists" 

export const parseRepoName = (text: string): Maybe<string> => {
    if (failedOutput(text)) return null

    // Remove possible .git suffix
    const cleanedUrl = text.trim().replace(/\.git$/, '')

    // Extract part after last /
    const parts = cleanedUrl.split('/')

    // Ensure we have enough parts (e.g., https://github.com/user/repo)
    if (parts.length < 1) return null

    return last(parts) || null
}

export const parseContributorEmails = (text: string): Maybe<string[]> => failedOutput(text) ? null : [...new Set(text.split("\n"))]

export const parseRepoBranches = (text: string): Maybe<string[]> => failedOutput(text) ? null : text
    .split("\n") // splits on new line
    .filter(l => !l.includes("->")) // doesn't include ref aliases
    .map(line => line.trim().replace(/^\* /, "")) // remove "* " from current branch
    .filter(Boolean); // remove empty lines

export const parseCommitHashes = (text: string): Maybe<string[]> => failedOutput(text) ? null : text
    .split("\n") // splits on new line
    .filter(line => !line.startsWith("The system cannot find the path specified.")) // for some reason this exists, but the rest of the logic still works???
    .filter(Boolean); // remove empty lines


export const parseCommitData = (text: string): Maybe<Readonly<{
    commitHash: string,
    commitTitle: string
    contributorName: string,
    description: string,
    dateString: string,
    involvedFiles: string[][]
}>> => {
    if (failedOutput(text)) return null
    
    const lines = text.split("\n").map(s => s.trim())
    if (lines.length < 5) return null

    const [commitHash, contributorName, dateString, commitTitle, description, ...fileLines] = lines

    const data = fileLines
        .filter(l => l != "")
        .map(line => {
        const parts = line.split(/\s+/)
        const status = parts[0]

        if (status.startsWith("R") || status.startsWith("C")) {
            // Rename or copy: status, from, to
            return [status[0], parts[1], parts[2]] 
        } else {
            // Simple: status, file
            return [status, parts[1]]
        }
    })

    return {
        commitHash,
        commitTitle,
        contributorName,
        description,
        dateString,
        involvedFiles: data
    }

}
export const parseFileDataFromCommit = (getFileContents: (text: string) => Promise<string>, getOldFileContents: (text: string) => Promise<string>) => async (data: string[]): Promise<Maybe<FileChanges>> => {

    const [changeString, ...rest] = data
    const [changeChar, ...likenessList] = changeString 
    const changeType = changeChar as ChangeType
    const likeness = Number.parseInt(likenessList.join())
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
                        oldFilePath: oldFilePath,
                        likeness: likeness
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




