#!/usr/bin/env python
from flask import Flask, jsonify, request, Response, make_response
import uuid
import os
from typing import List, Tuple
import subprocess
import shutil
import ast
import re

PORT = 8002
app = Flask(__name__)

ALLOWED_LITERAL_PATTERN = r'\[([^\(\[\{\)\]\}]|(\"[^\"]*\")|(\'[^\']*\'))*\]'
ALLOWED_LITERAL_RE = re.compile(ALLOWED_LITERAL_PATTERN)


@app.route('/execute', methods=['POST'])
def execute():
    """ Executes a given python script with some data.csv file available to it. """
    print("\t> Received new /execute request")

    if "data.csv" not in request.files:
        return jsonify({"error": "Missing 'data.csv' file"}), 400
    if "script.py" not in request.files:
        return jsonify({"error": "Missing 'script.py' file"}), 400

    script = request.files["script.py"]
    data = request.files["data.csv"]

    if not script:
        return jsonify({"error": "Given file 'script.py' was not valid"}), 400
    if not data:
        return jsonify({"error": "Given file 'data.csv' was not valid"}), 400

    # Put the files into an fs to be used by the executor
    run_uuid = uuid.uuid4()

    execution_dir = os.path.join("/tmp", str(run_uuid))
    os.mkdir(execution_dir)

    script_path = os.path.join(execution_dir, "script.py")
    data_path = os.path.join(execution_dir, "data.csv")

    script.save(script_path)
    data.save(data_path)

    # Perform execution
    result = make_response(exec_in_sandbox(["python3", "/home/python/script.py"], cwd=execution_dir))

    # Clean up temporary resources
    shutil.rmtree(execution_dir, True)
    return result


def get_last_lines(s: str, n: int = 50) -> str:
    lines = s.splitlines()
    return "\n".join(lines[-n:])


def exec_in_sandbox(command: List[str],
                    cwd: str | None = None):
    """
    Executes a given shell command in one of our custom nix-based sandbox containers.
    :param command: The command to run as a list of strings, where each string is a space-separated component of the
    shell command
    :param cwd: The directory to use as current directory when executing the command. All files at this directory will
    be made available to the sandbox environment at /home/python/
    :return: The API response for the execution
    """
    try:
        result = subprocess.run(
            ['python-executor-env'] + command,
            capture_output=True,
            start_new_session=True,
            timeout=1.0,
            text=True,
            cwd=cwd,
        )
    except subprocess.TimeoutExpired as ex:
        stderr = ex.stderr.decode('ascii') if ex.stderr else ""
        stdout = ex.stdout.decode('ascii') if ex.stdout else ""

        # Timed out!
        return jsonify({
            "error": "Program timed out!",
            "stdout": get_last_lines(stdout),
            "stderr": get_last_lines(stderr)
        }), 400

    # Ensure the program exited without error
    if result.returncode < 0:
        return jsonify({
            "error": f"Program returned with error code {result.returncode}",
            "stderr": get_last_lines(result.stderr),
            "stdout": get_last_lines(result.stdout),
        }), 400

    # Try parse any csv data from result.stdout
    stdout = result.stdout
    lines = stdout.splitlines()

    # Collect any output lines that have a comma in them, and treat them as csv rows
    data_rows = []
    for line in lines:
        if not ((line.startswith('(') and line.endswith(')')) or (line.startswith('[') and line.endswith(']'))):
            continue

        # Convert tuples to arrays
        if line.startswith('(') and line.endswith(')'):
            _, line = line.split('(', 1)
            line, _ = line.rsplit(')', 1)
            line = '[' + line + ']'


        # Protect against stack overflow attacks by only allowing flat structures
        if not ALLOWED_LITERAL_RE.match(line):
            return jsonify({
                "error": f"Program returned a literal with nested structures (not allowed).",
                "stderr": get_last_lines(result.stderr),
                "stdout": "\n".join(lines[-50:]),
            }), 400

        try:
            line_list = ast.literal_eval(line)
        except (ValueError, SyntaxError):
            # Ignore invalid lists
            continue

        data_rows.append(line_list)

    # Return result
    return jsonify({
        "stderr": get_last_lines(result.stderr),
        "stdout": "\n".join(lines[-50:]),
        "data": data_rows,
    }), 200 if result.stderr is None or len(result.stderr.strip()) == 0 else 400


if __name__ == '__main__':
    app.run(host='0.0.0.0', debug=True, port=PORT)
