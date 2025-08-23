import { Meteor } from "meteor/meteor";
import { spawn } from "child_process";
import path from "path";

Meteor.methods({
    runScaling({ users, config }) {
        return new Promise((resolve, reject) => {
            // Absolute path to Python script in source folder
            const scriptPath = path.join(
                "/Users/yoonusnaz/UNIVERSITY/SEM 2/FIT3170_S2/2025W2-Commitment/commitment/",
                "ScaleGrades.py"
            );
            console.log("Running Python script:", scriptPath);

            const py = spawn("python3", [scriptPath]);

            let output = "";
            let errorOutput = "";

            py.stdout.on("data", (chunk) => {
                const text = chunk.toString();
                output += text;
                console.log("[Python stdout]:", text.trim());
            });

            py.stderr.on("data", (chunk) => {
                const text = chunk.toString();
                errorOutput += text;
                console.error("[Python stderr]:", text.trim());
            });

            py.on("close", (code) => {
                console.log(`Python process exited with code ${code}`);
                if (code !== 0) {
                    reject(new Meteor.Error("python-failed", errorOutput));
                } else {
                    try {
                        const result = JSON.parse(output);
                        console.log("Python output parsed successfully:", result);
                        resolve(result);
                    } catch (e) {
                        console.error("Failed to parse Python output:", output);
                        reject(new Meteor.Error("parse-failed", output));
                    }
                }
            });

            py.stdin.write(JSON.stringify({ users, config }));
            py.stdin.end();
        });
    },
});