import { exec, spawn } from "child_process";
import fs from "fs";
import * as fs_promise from "fs/promises";
import path from "path";

export type Command = Readonly<{
  cmd: string;
  onSuccess: (command: string) => string;
  onFail: (command: string, error: Error) => string;
  onStdFail: (command: string, error: string) => string;
  shouldLog: boolean;
}>;

export type CommandResult = Readonly<{
  result: string;
  error: Error | null;
  stdError: string | null;
}>;

export const successful = (res: CommandResult): boolean =>
  res.error == null && res.stdError == null;

export const assertSuccess =
  (errMsg: string) =>
  (res: CommandResult): string => {
    if (!successful(res)) throw Error(errMsg);
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

export const guaranteeExecution =
  (cwd: string) =>
  (f: Command): Promise<CommandResult> =>
    new Promise((resolve, reject) => {
      exec(f.cmd, { cwd: cwd }, (error: Error | null, stdout: string, stderr: string) => {
        if (stderr) {
          if (f.shouldLog) console.error(f.onStdFail(f.cmd, stderr));

          return resolve({
            ...defaultResult,
            result: stdout,
            stdError: stderr,
          });
        } else if (error) {
          if (f.shouldLog) console.error(f.onFail(f.cmd, error));

          return resolve({
            ...defaultResult,
            result: stdout,
            error: error,
            stdError: stderr,
          });
        } else {
          if (f.shouldLog) console.log(f.onSuccess(f.cmd));

          return resolve({
            ...defaultResult,
            result: stdout,
          });
        }
      });
    });

export const executeCommand =
  (cwd: string) =>
  (f: Command): Promise<CommandResult> =>
    new Promise((resolve, reject) => {
      const [command, ...args] = f.cmd.split(" ");
      const child = spawn(command, args, { cwd });

      // memory mutability is used here as it reduces overhead and increases performance as opposed to RXJS observable acculuation in state
      // this is one of the only times I will not use pure consts, but because it works with side effect code its fine
      // and it should also spare on some performance
      const stdoutChunks: Buffer[] = [];
      const stderrChunks: Buffer[] = [];

      child.stdout.on("data", (chunk: Buffer) => {
        stdoutChunks.push(chunk);
      });

      child.stderr.on("data", (chunk: Buffer) => {
        stderrChunks.push(chunk);
      });

      child.on("error", (error: Error) => {
        const stdout = Buffer.concat(stdoutChunks).toString();
        const stderr = Buffer.concat(stderrChunks).toString();

        if (f.shouldLog) console.error(f.onFail(f.cmd, error));
        resolve({
          ...defaultResult,
          result: stdout,
          error,
          stdError: stderr || null,
        });
      });

      child.on("close", (code) => {
        const stdout = Buffer.concat(stdoutChunks).toString();
        const stderr = Buffer.concat(stderrChunks).toString();

        if (stderr) {
          if (f.shouldLog) console.error(f.onStdFail(f.cmd, stderr));
          return resolve({
            ...defaultResult,
            result: stdout,
            stdError: stderr,
          });
        }

        if (code !== 0) {
          const err = new Error(`Process exited with code ${code}`);
          if (f.shouldLog) console.error(f.onFail(f.cmd, err));
          return resolve({
            ...defaultResult,
            result: stdout,
            error: err,
            stdError: stderr || null,
          });
        }

        if (f.shouldLog) console.log(f.onSuccess(f.cmd));
        resolve({
          ...defaultResult,
          result: stdout,
        });
      });
    });
