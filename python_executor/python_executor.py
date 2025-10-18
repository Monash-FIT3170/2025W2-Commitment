from flask import Flask, jsonify, request, Response, make_response
import uuid
import os
from typing import List, Tuple
import subprocess
import shutil

PORT = 8002
app = Flask(__name__)


@app.route('/execute', methods=['POST'])
def execute():
    """ Executes a given python script with some data.csv file available to it. """
    print("Received new /execute request...")

    res = subprocess.run(
        ['nix-build', '/projects/python_executor', '-o', '/home/python/result'],
        stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True
    )
    print(res.stdout, res.stderr)
    print("updated")

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


def exec_in_sandbox(command: List[str],
                    overlays: List[Tuple[str, str]] | None = None,
                    cwd: str | None = None):
    overlay_arg_sets = [["--overlay-src", x[0], "--tmp-overlay", x[1]] for x in overlays] if overlays else []
    overlay_args = [arg for sublist in overlay_arg_sets for arg in sublist]

    try:
        result = subprocess.run(
            ['/home/python/result/bin/python-executor-env'] + overlay_args + command,
            capture_output=True,
            start_new_session=True,
            timeout=1,
            text=True,
            cwd=cwd,
        )
    except subprocess.TimeoutExpired as ex:
        stderr = ex.stderr.decode('ascii') if ex.stderr else ""
        stdout = ex.stdout.decode('ascii') if ex.stdout else ""

        # Timed out!
        return jsonify({
            "error": "Program timed out!",
            "stderr": stderr,
            "stdout": stdout,
        }), 400

    # Ensure the program exited without error
    if result.returncode < 0:
        return jsonify({
            "error": f"Program returned with error code {result.returncode}",
            "stderr": result.stderr,
            "stdout": result.stdout,
        }), 400


    # Return result
    return jsonify({
        "stderr": result.stderr,
        "stdout": result.stdout,
        "data": result.stdout,
    }), 200


if __name__ == '__main__':
    app.run(host='0.0.0.0', debug=True, port=PORT)
