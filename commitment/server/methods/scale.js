import { Meteor } from "meteor/meteor";
import { spawn } from "child_process";

Meteor.methods({
    runScaling({ users, config }) {
        return new Promise((resolve, reject) => {
            const py = spawn("python3", [
                "./server/methods/ScaleGrades.py"
            ]);

            let output = "";
            let errorOutput = "";

            py.stdout.on("data", (chunk) => (output += chunk.toString()));
            py.stderr.on("data", (chunk) => (errorOutput += chunk.toString()));

            py.on("close", (code) => {
                if (code !== 0) {
                    console.error("Python exited with code", code, errorOutput);
                    reject(new Meteor.Error("python-failed", "Python script failed"));
                } else {
                    try {
                        resolve(JSON.parse(output));
                    } catch (e) {
                        reject(new Meteor.Error("parse-failed", "Failed to parse Python output"));
                    }
                }
            });

            // Send JSON to Python stdin
            py.stdin.write(JSON.stringify({ users, config }));
            py.stdin.end();
        });
    },
});