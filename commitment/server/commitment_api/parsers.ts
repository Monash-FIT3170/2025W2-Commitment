import { CommandResult, successful } from './command';
import { last } from './helpers';
import {
  FileChanges,
  ChangeData,
  ChangeType,
} from './types';

export type Maybe<T> = T | null

export const fromMaybe = <T>(m: Maybe<T>, error: string): T => 
    m === null ? (() => { throw new Error(error); })() : m

export const parse = (s: string) => <T>(parser: (text: string) => Maybe<T>): T => 
    fromMaybe(parser(s), `parser \"${parser.name}\" failed to parse text:\n${s}`)

// parses and executes p at the same time (less efficient if this is called multiple times but has good syntax for inline statements)
export const parsePromise = (p: Promise<string>) => async <T>(parser: (text: string) => Maybe<T>): Promise<T> => 
    parse(await p)(parser)

// executes p and then captures it for use in multiple parsers (more efficient as it does not have multiple executions of the same promise)
export const parsePromiseHold = async (p: Promise<string>): Promise<<T>(parser: (text: string) => Maybe<T>) => T> => {
    const s = await p
    return <T>(parser: (text: string) =>  Maybe<T>): T => parse(s)(parser)
} 

// prioritises stderror, then error, then a successful message
export const getParsableStringFromCmd = (res: CommandResult): string => ((res.stdError != null) ? res.stdError : ((res.error != null) ? res.error.message : res.result));

export const parseCmd = (p: Promise<CommandResult>): <T>(parser: (text: string) => Maybe<T>) => Promise<T> => 
    parsePromise(p.then(getParsableStringFromCmd))

export const parseCmdHold = (p: Promise<CommandResult>): Promise<(<T>(parser: (text: string) => Maybe<T>) => T)> => 
    parsePromiseHold(p.then(getParsableStringFromCmd))

export const assertSuccess = (res: CommandResult) => 
    successful(res) ? (() => { throw new Error(getParsableStringFromCmd(res)); })() : null

export const failedOutput = (text: string): boolean => ['fatal:', 'error:', 'could not', 'not a git repository']
  .map((s) => text.startsWith(s))
  .reduce((p, n) => p || n);

export const exactText = (text: string): Maybe<string> => failedOutput(text) ? null : text

export const parseRepoExists = (text: string): Maybe<string> => failedOutput(text) ? null : "repo exists" 

export const parseRepoName = (text: string): Maybe<string> => {
  if (failedOutput(text)) return null;

  // Remove possible .git suffix
  const cleanedUrl = text.trim().replace(/\.git$/, '');

  // Extract part after last /
  const parts = cleanedUrl.split('/');

  // Ensure we have enough parts (e.g., https://github.com/user/repo)
  if (parts.length < 1) return null;

  return last(parts) || null;
};

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

type InbetweenCommitData = Readonly<{
    commitHash: string,
    commitTitle: string,
    contributorName: string,
    description: string,
    dateString: string,
    involvedFiles: string[][]
}>

const delim = "\n|||END|||"

const trimTextWithNewLine = (s: string) => s
    .split("\n")
    .map(l => l.trim())
    .join("\n")

export const parseCommitData = (text: string): Maybe<InbetweenCommitData> => {
    if (failedOutput(text)) return null

    const blocks = text.split(delim)
    if (blocks.length < 5) return null

    const [commitHash, contributorName, dateString, commitTitle, descriptionOrEmpty, rest] = blocks

    // Handle case where there's no description (empty string)
    const description = descriptionOrEmpty || ""

    // Join the rest into a flat list of file lines
    const fileLines = rest
        .split("\n")
        .map(l => l.trim())
        .filter(l => l !== '')

    const involvedFiles = fileLines.map(line => {
        const parts = line.split(/\s+/)
        const status = parts[0]

        if (status.startsWith("R") || status.startsWith("C")) {
            // Rename or copy: status, from, to
            return [status, parts[1], parts[2]]
        } else {
            // Normal add/modify/delete: status, path
            return [status, parts[1]]
        }
    })

    return {
        commitHash: trimTextWithNewLine(commitHash),
        commitTitle: trimTextWithNewLine(commitTitle),
        contributorName: trimTextWithNewLine(contributorName),
        description: trimTextWithNewLine(description),
        dateString: trimTextWithNewLine(dateString),
        involvedFiles
    }
}


export const parseFileDataFromCommit = (getFileContents: (text: string) => Promise<string>, getOldFileContents: (text: string) => Promise<string>) => async (data: string[]): Promise<Maybe<FileChanges>> => {
  const [changeString, ...rest] = data;
  const [changeChar, ...likenessList] = changeString;
  const changeType = changeChar as ChangeType;
  const likeness = Number.parseInt(likenessList.join());
  const newFile = last(rest) as string;

  const getChange = async (): Promise<ChangeData> => {
		// if renamed or copied get info for the previous file path
		if (rest.length > 1) {
			// get the old filepath
			const oldFilePath = rest[0];
			// if not a modify then we dont need to grab old file data for a diff
			if (changeType in ['R', 'C']) {
				return {
					char: changeType,
					extra: {
						oldFilePath,
						likeness,
					},
				};
			} if (changeType === 'M') {
				// grab old file contents
				const oldFileContents = {
					contents: await getOldFileContents(oldFilePath),
					filepath: oldFilePath,
				};

				return {
					char: changeType,
					extra: {
						previousFile: oldFileContents,
					},
				};
			}
			// we should not be here, raise error or smth idk lol
		}
		throw Error("getChange in changedata shat itself")
	}

    // if the file is deleted, do not try to get its file contents as it probably does not exist
    const updatedFileContents = changeType !== "D" ? await getFileContents(newFile) : ""

    const fileContents = {
        contents: updatedFileContents,
        filepath: newFile
    }
    
    return ({
        file: fileContents,
        changes: await getChange()
    } as FileChanges)
}
