
import { exec } from 'child_process';

export type Command = Readonly<{
    cmd: string, 
    onSuccess: () => void,
    onFail: (error: Error) => void,
    onStdFail: (error: string) => void,
    shouldLog: boolean
}>;

export const defaultStdFail = (stderr: string) => { console.warn(`⚠️ STDERR: ${stderr}`) }

export const doNotLogData: Command = {
    cmd: "",
    onSuccess: () => {},
    onFail: (e: Error) => {},
    onStdFail: defaultStdFail,
    shouldLog: false
}

export const executeCommand = (cwd: string) => (f: Command): Promise<string> => {
    return new Promise((resolve, reject) => {
        exec(f.cmd, 
            { cwd: cwd },
            (error: Error | null, stdout: string, stderr: string) => {

                if (stderr) { 
                    if (f.shouldLog) f.onStdFail(stderr)
                    return reject(stderr);

                } else if (error) {
                    if (f.shouldLog) f.onFail(error)
                    return reject(error);

                } else {
                    if (f.shouldLog) f.onSuccess()
                    return resolve(stdout)
                }
            }
        )
    })
}
