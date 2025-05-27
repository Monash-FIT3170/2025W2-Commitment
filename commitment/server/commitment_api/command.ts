import { Observable } from "rxjs";
import { execSync } from 'child_process';
import fs from "fs"
import * as fs_promise from "fs/promises";
import path from "path";

export type Command = Readonly<{
    cmd: string, 
    onSuccess: (command: string) => string,
    onFail:    (command: string, error: Error)  => string,
    onStdFail: (command: string, error: string) => string,
    shouldLog: boolean
}>;

export type CommandResult = Readonly<{
    result: string
    error: Error | null,
    stdError: string | null
}>;

export const successful = (res: CommandResult): boolean => res.error == null && res.stdError == null

const defaultResult = {
    result: null,
    error: null,
    stdError: null
}

export const defaultSuccess = (command: string)                 => `✅  Command succeeded: ${command}`
export const defaultFail    = (command: string, e: Error)       => `❌  Command failed: ${command}\n Error: ${e.message}`
export const defaultStdFail = (command: string, stderr: string) => `⚠️  STDERR\nCommand: ${command}\n Error: ${stderr}`

export const logData: Command = {
    cmd: "",
    onSuccess: defaultSuccess,
    onFail:    defaultFail,
    onStdFail: defaultStdFail,
    shouldLog: true
}

export const doNotLogData: Command = {
    ...logData,
    shouldLog: false
}

export const executeCommand = (cwd: string) => (f: Command): () => CommandResult => {
    return (): CommandResult => {
        try {
            const stdout = execSync(f.cmd, { cwd, encoding: 'utf-8' })

            if (f.shouldLog) console.log(f.onSuccess(f.cmd))

            return {
                ...defaultResult,
                result: stdout,
            }

        } catch (error: any) {

            const stderr = error.stderr?.toString?.() || ""
            const stdout = error.stdout?.toString?.() || ""

            if (stderr && f.shouldLog) {
                console.error(f.onStdFail(f.cmd, stderr));
            } else if (f.shouldLog) {
                console.error(f.onFail(f.cmd, error));
            }

            return {
                ...defaultResult,
                result: stdout,
                error,
                stdError: stderr,
            }
        }
    };
};

export const doesFilePathExist = (filepath: string): boolean => fs.existsSync(filepath)

export const createFilePath = (filepath: string): boolean => {
    try {
        const dir = path.dirname(filepath); // Get the folder from the file path

        // Check if the directory exists; create it if it doesn't
        if (!fs.existsSync(dir)) {
          fs.mkdirSync(dir, { recursive: true }); // 'recursive' creates all missing directories
        }
      
        return true;

    } catch (error: any) {
        return false
    }
}

export const deleteAllFromDirectory = async (dirPath: string) => {
    const entries = await fs_promise.readdir(dirPath, { withFileTypes: true })

    await Promise.all(entries.map(async (entry) => {
        const fullPath = path.join(dirPath, entry.name);
    
        if (entry.isDirectory()) {
            // is a subdirectory, so delete this as well
            return fs_promise.rm(fullPath, { recursive: true, force: true });
        } else {
            // just a file, so delete
            return fs_promise.unlink(fullPath);
        }
        }
    ))
}

