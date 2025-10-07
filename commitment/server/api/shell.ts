import { exec } from "child_process";
import * as fs_promise from "fs/promises";
import fs from "fs";
import os from "os";
import path from "path";

export type Command = Readonly<{
  cmd: string;
  onSuccess: (command: string) => string;
  onFail: (command: string, error: Error) => string;
  onStdFail: (command: string, error: string) => string;
  shouldLog: boolean;
}>;

export type CommandResult = Readonly<{
  cmd: Command;
  result: string;
  error: Error | null;
  stdError: string | null;
}>;

export const anyFailedOuptut = (msg: string): boolean =>
  [
    "fatal:",
    "error:",
    "could not",
    "not a git repository",
    "Process exited with code",
    "Encountered error",
    "Process timed out",
  ]
    .map(msg.startsWith)
    .reduce((acc: boolean, b: boolean) => acc || b, false);

export const successful = (res: CommandResult): boolean =>
  res.stdError !== null ? !anyFailedOuptut(res.stdError) : true && res.error !== null;

export const getErrorMsg = (res: CommandResult): string => {
  if (res.error) return res.error.message;
  else if (res.stdError) return res.stdError;
  throw Error(`Command was successful: ${res.cmd}`);
};

export const assertSuccess =
  (errMsg: string) =>
  (res: CommandResult): string => {
    if (!successful(res))
      throw Error(
        `${errMsg}: \nError when running command "${res.cmd.cmd}": \n${getErrorMsg(res)}`
      );
    return res.result;
  };

const defaultResult = {
  result: null,
  error: null,
  stdError: null,
};

export const defaultSuccess = (command: string) => `✅  Command succeeded: ${command}`;
export const defaultFail = (command: string, e: Error) =>
  `❌  Command failed: ${command}\n Error: ${e.message}`;
export const defaultStdFail = (command: string, stderr: string) =>
  `⚠️  STDERR\nCommand: ${command}\n Error: ${stderr}`;

export const logData: Command = {
  cmd: "",
  onSuccess: defaultSuccess,
  onFail: defaultFail,
  onStdFail: defaultStdFail,
  shouldLog: true,
};

export const doNotLogData: Command = {
  ...logData,
  shouldLog: false,
};

export const executeCommand =
  (cwd: string) =>
  (f: Command): Promise<CommandResult> =>
    new Promise((resolve, reject) => {
      exec(f.cmd, { cwd: cwd }, (error: Error | null, stdout: string, stderr: string) => {
        if (stderr) {
          if (f.shouldLog) console.error(f.onStdFail(f.cmd, stderr));

          return resolve({
            ...defaultResult,
            cmd: f,
            result: stdout,
            stdError: stderr,
          });
        } else if (error) {
          if (f.shouldLog) console.error(f.onFail(f.cmd, error));

          return resolve({
            ...defaultResult,
            cmd: f,
            result: stdout,
            error: error,
            stdError: stderr,
          });
        } else {
          if (f.shouldLog) console.log(f.onSuccess(f.cmd));

          return resolve({
            ...defaultResult,
            cmd: f,
            result: stdout,
          });
        }
      });
    });

/**
 * a function which creates a uniquely named directory with the prefix
 * @param baseDir the base directory of the path
 * @param prefix  prefix of the directory after the path
 * @returns
 */
export const createDirectory =
  (baseDir: string) =>
  async (prefix: string): Promise<string> => {
    // Ensure the base directory is absolute
    // mkdir requires the prefix to be a full path
    const absBase = path.resolve(baseDir);
    return await fs_promise.mkdir(path.join(absBase, prefix), { recursive: true });
  };

export const createTempDirectory = createDirectory(os.tmpdir());

export const deleteAllFromDirectory = async (dirPath: string) => {
  const entries: fs.Dirent[] = await fs_promise.readdir(dirPath, {
    withFileTypes: true,
  });
  return Promise.all([
    ...entries.map(async (entry) => {
      const fullPath = path.join(dirPath, entry.name);

      if (entry.isDirectory()) {
        // is a subdirectory, so delete this as well
        return fs_promise.rm(fullPath, { recursive: true, force: true });
      } else {
        // just a file, so delete
        return fs_promise.unlink(fullPath);
      }
    }),
    fs_promise.rmdir(dirPath, { recursive: true }), // removes dirPath as well as all subdirectories
  ]);
};
