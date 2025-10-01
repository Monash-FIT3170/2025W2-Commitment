import { exec } from "child_process";
import * as fs_promise from "fs/promises";
import fs from "fs";
import os from "os";
import path from "path";
import { log } from "console";

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

export const successful = (res: CommandResult): boolean => res.error == null;

export const getErrorMsg = (res: CommandResult): string => {
  if (res.error) return res.error.message;
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
      console.log("Current working directory:", cwd);
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
    // mkdtemp requires the prefix to be a full path
    await fs_promise.mkdir(baseDir, { recursive: true });
    console.log("directory created: ", baseDir);
    console.log("directory: ", path.join(path.resolve(baseDir), prefix));
    const tmpDir = await fs_promise.mkdtemp(path.join(path.resolve(baseDir), prefix));
    console.log("temp directory created: ", baseDir, prefix);
    return tmpDir;
  };

export const createTempDirectory = createDirectory(os.tmpdir());

export const deleteAllFromDirectory = async (dirPath: string) => {
  const entries: fs.Dirent[] = await fs_promise.readdir(dirPath, { withFileTypes: true });
  const selfP = async () => fs_promise.rmdir(dirPath, { recursive: true });
  return Promise.all([
    ...entries.map(async (entry) => {
      const fullPath = path.join(dirPath, entry.name);

      if (entry.isDirectory()) {
        // is a subdirectory, so delete this as well
        fs_promise.rm(fullPath, { recursive: true, force: true });
      } else {
        // just a file, so delete
        fs_promise.unlink(fullPath);
      }
    }),
    selfP(),
  ]);
};
