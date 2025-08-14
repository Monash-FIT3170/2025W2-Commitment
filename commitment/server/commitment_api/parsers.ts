import { CommandResult, successful } from './command';
import { last } from './helpers';
import {
  FileChanges,
  ChangeData,
  ChangeType,
} from './types';

export type Maybe<T> = T | null

export const fromMaybe = <T>(m: Maybe<T>, error: string): Promise<T> => (m === null ? Promise.reject(new Error(error)) : Promise.resolve(m));

export const parse = (s: string) => <T>(parser: (text: string) => Maybe<T>): Promise<T> => fromMaybe(parser(s), `parser \"${parser.name}\" failed to parse text:\n${s}`);

// executes p and then captures it for use in multiple parsers (more efficient as it does not have multiple executions of the same promise)
export const parsePromise = async (p: Promise<string>): Promise<<T>(parser: (text: string) => Maybe<T>) => Promise<T>> => {
  const s = await p;
  return <T>(parser: (text: string) => Maybe<T>): Promise<T> => parse(s)(parser);
};

// parses and executes p at the same time (less efficient if this is called multiple times but has good syntax for inline statements)
export const parsePromiseImmediate = (p: Promise<string>) => async <T>(parser: (text: string) => Maybe<T>): Promise<T> => parse(await p)(parser);

// prioritises stderror, then error, then a successful message
export const getParsableStringFromCmd = (res: CommandResult): string => ((res.stdError != null) ? res.stdError : ((res.error != null) ? res.error.message : res.result));

export const parseCmd = (p: Promise<CommandResult>): Promise<(<T>(parser: (text: string) => Maybe<T>) => Promise<T>)> => parsePromise(p.then(getParsableStringFromCmd));

export const parseCmdImmediate = (p: Promise<CommandResult>): <T>(parser: (text: string) => Maybe<T>) => Promise<T> => parsePromiseImmediate(p.then(getParsableStringFromCmd));

export const parseSuccess = (res: Promise<CommandResult>): Promise<string> => res.then((res) => (successful(res) ? Promise.reject(new Error(getParsableStringFromCmd(res))) : Promise.resolve(res.result)));

export const failedOutput = (text: string): boolean => {
  console.log('failedOutput checking text:', JSON.stringify(text));
  
  // Handle Git file path errors gracefully - these are not failures
  if (text.includes('fatal: path') && text.includes('exists on disk, but not in')) {
    console.log('Detected Git file path error - not a failure');
    return false;
  }
  
  const patterns = ['fatal:', 'error:', 'could not', 'not a git repository'];
  const result = patterns
    .map((s) => text.startsWith(s))
    .reduce((p, n) => p || n);
    
  console.log('failedOutput patterns checked:', patterns);
  console.log('failedOutput result:', result);
  
  return result;
};

export const exactText = (text: string): Maybe<string> => {
  // Handle Git file path errors gracefully
  if (text.includes('fatal: path') && text.includes('exists on disk, but not in')) {
    return ''; // Return empty string for missing files instead of null
  }
  return (failedOutput(text) ? null : text);
};

export const parseRepoExists = (text: string): Maybe<string> => {
  console.log('parseRepoExists received text:', JSON.stringify(text));
  console.log('Text length:', text.length);
  console.log('Text trimmed:', text.trim());
  
  // Special case: Large repositories that cause buffer overflow
  if (text === 'REPO_EXISTS_LARGE') {
    console.log('Large repository detected - buffer overflow case');
    return 'repo exists';
  }
  
  if (failedOutput(text)) {
    console.log('failedOutput returned true');
    return null;
  }
  
  // Check if the command output indicates repo not found
  if (text.trim() === '') {
    console.log('Text is empty after trim');
    return null;
  }
  
  // If we have any output from git ls-remote, the repo exists
  // git ls-remote returns refs (branches/tags) if the repo exists
  console.log('Repo exists - returning success');
  return 'repo exists';
};

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

export const parseContributorEmails = (text: string): Maybe<string[]> => (failedOutput(text) ? null : [...new Set(text.split('\n'))]);

export const parseRepoBranches = (text: string): Maybe<string[]> => (failedOutput(text) ? null : text
  .split('\n') // splits on new line
  .filter((l) => !l.includes('->')) // doesn't include ref aliases
  .map((line) => line.trim().replace(/^\* /, '')) // remove "* " from current branch
  .filter(Boolean)); // remove empty lines

export const parseCommitHashes = (text: string): Maybe<string[]> => (failedOutput(text) ? null : text
  .split('\n') // splits on new line
  .filter((line) => !line.startsWith('The system cannot find the path specified.')) // for some reason this exists, but the rest of the logic still works???
  .filter(Boolean)); // remove empty lines

export const parseCommitData = (text: string): Maybe<Readonly<{
    commitHash: string,
    commitTitle: string,
    contributorName: string,
    description: string,
    dateString: string,
    involvedFiles: string[][]
}>> => {
  if (failedOutput(text)) return null;

  const lines = text.split('\n').map((s) => s.trim());
  if (lines.length < 5) return null;

  // Fix destructuring: git show format gives: hash, author, date, subject, body, then file changes
  const [commitHash, contributorName, dateString, commitTitle, ...remainingLines] = lines;
  
  // Extract description from remaining lines (everything before file changes)
  const description = remainingLines[0] || '';
  
  // Filter out empty lines and get file changes
  const fileLines = remainingLines.slice(1).filter((l) => l != '');

  const data = fileLines
    .map((line) => {
      const parts = line.split(/\s+/);
      const status = parts[0];

      if (status.startsWith('R') || status.startsWith('C')) {
        // Rename or copy: status, from, to
        return [status[0], parts[1], parts[2]];
      }
      // Simple: status, file
      return [status, parts[1]];
    });

  return {
    commitHash,
    commitTitle,
    contributorName,
    description,
    dateString,
    involvedFiles: data,
  };
};
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
      if (['R', 'C'].includes(changeType)) {
        return {
          char: changeType,
          extra: {
            oldFilePath,
            likeness,
          },
        };
      } else if (changeType === 'M') {
        // grab old file contents - handle missing files gracefully
        try {
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
        } catch (error) {
          // If old file doesn't exist, return modify without previous file
          return {
            char: changeType,
            extra: null,
          };
        }
      }
      // we should not be here, raise error or smth idk lol
    }
    // if not just return the change type as no additional information is needed
    return {
      char: changeType,
      extra: null,
    };
  };

  try {
    const updatedFileContents = await getFileContents(newFile);
    
    // Skip processing if file contents are empty (file doesn't exist in this commit)
    if (!updatedFileContents || updatedFileContents.trim() === '') {
      return null;
    }

    const fileContents = {
      contents: updatedFileContents,
      filepath: newFile,
    };

    return ({
      file: fileContents,
      changes: await getChange(),
    } as FileChanges);
  } catch (error) {
    // If we can't get file contents, skip this file
    console.warn(`Skipping file ${newFile} due to error:`, error);
    return null;
  }
};
