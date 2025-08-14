import { exec } from 'child_process';
import fs from 'fs';
import * as fs_promise from 'fs/promises';
import path from 'path';

export type Command = Readonly<{
    cmd: string,
    onSuccess: (command: string) => string,
    onFail: (command: string, error: Error) => string,
    onStdFail: (command: string, error: string) => string,
    shouldLog: boolean
}>;

export type CommandResult = Readonly<{
    result: string
    error: Error | null,
    stdError: string | null
}>;

export const successful = (res: CommandResult): boolean => res.error == null && res.stdError == null;

const defaultResult = {
  result: null,
  error: null,
  stdError: null,
};

export const defaultSuccess = (command: string) => `✅  Command succeeded: ${command}`;
export const defaultFail = (command: string, e: Error) => `❌  Command failed: ${command}\n Error: ${e.message}`;
export const defaultStdFail = (command: string, stderr: string) => `⚠️  STDERR\nCommand: ${command}\n Error: ${stderr}`;

export const logData: Command = {
  cmd: '',
  onSuccess: defaultSuccess,
  onFail: defaultFail,
  onStdFail: defaultStdFail,
  shouldLog: true,
};

export const doNotLogData: Command = {
  ...logData,
  shouldLog: false,
};

export const executeCommand = (cwd: string) => (f: Command): Promise<CommandResult> => new Promise((resolve, reject) => {
  // Add timeout to prevent hanging on authentication prompts
  const timeout = 30000; // 30 seconds timeout
  
  const child = exec(f.cmd, { 
    cwd,
    timeout, // Add timeout
    maxBuffer: 10 * 1024 * 1024 // 10MB buffer (increased from 1MB)
  }, (error: Error | null, stdout: string, stderr: string) => {
    console.log(`Command executed: ${f.cmd}`);
    console.log(`stdout: ${JSON.stringify(stdout)}`);
    console.log(`stderr: ${JSON.stringify(stderr)}`);
    console.log(`error: ${error ? error.message : 'null'}`);
    
    // Handle buffer overflow specifically
    if (error && error.message.includes('maxBuffer')) {
      console.log('Buffer overflow detected - repository is very large');
      // For buffer overflow, we consider it a success since it means the repo exists
      return resolve({
        ...defaultResult,
        result: 'REPO_EXISTS_LARGE', // Special marker for large repos
        error: null,
        stdError: 'Repository is very large (buffer overflow)'
      });
    }
    
    if (stderr) {
      if (f.shouldLog) console.error(f.onStdFail(f.cmd, stderr));

      return resolve({
        ...defaultResult,
        result: stdout,
        stdError: stderr,
      });
    } if (error) {
      if (f.shouldLog) console.error(f.onFail(f.cmd, error));

      return resolve({
        ...defaultResult,
        result: stdout,
        error,
        stdError: stderr,
      });
    }
    if (f.shouldLog) console.log(f.onSuccess(f.cmd));

    return resolve({
      ...defaultResult,
      result: stdout,
    });
  });

  // Handle timeout
  child.on('error', (error) => {
    if (f.shouldLog) console.error(`Command timed out or failed: ${f.cmd}`, error);
    resolve({
      ...defaultResult,
      result: '',
      error: new Error(`Command timed out after ${timeout}ms: ${f.cmd}`),
      stdError: 'Command timed out'
    });
  });
});

export const doesFilePathExist = (filepath: string): boolean => fs.existsSync(filepath);

export const createFilePath = (filepath: string): boolean => {
  try {
    const dir = path.dirname(filepath); // Get the folder from the file path

    // Check if the directory exists; create it if it doesn't
    if (!fs.existsSync(dir)) {
      fs.mkdirSync(dir, { recursive: true }); // 'recursive' creates all missing directories
    }

    return true;
  } catch (error: any) {
    return false;
  }
};

export const deleteAllFromDirectory = async (dirPath: string) => {
  const entries = await fs_promise.readdir(dirPath, { withFileTypes: true });

  await Promise.all(entries.map(async (entry) => {
    const fullPath = path.join(dirPath, entry.name);

    if (entry.isDirectory()) {
      // is a subdirectory, so delete this as well
      return fs_promise.rm(fullPath, { recursive: true, force: true });
    }
    // just a file, so delete
    return fs_promise.unlink(fullPath);
  }));
};
