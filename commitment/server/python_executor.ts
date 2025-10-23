import dotenv from "dotenv";
import {
  AliasEmail,
  PythonExecutorResponse, PythonExecutorScalingResponse,
  type UserScalingSummary
} from "@api/types";
import {Meteor} from "meteor/meteor";
import {StudentAlias} from "@api/alias_configs";

/**
 * Checks that a given value is actually an object, not an array, or a function, or something else.
 * @param value The value to check is an object
 * @returns True if value is an object, false otherwise.
 */
function isObject(value: unknown): boolean {
  return value !== null && typeof value === "object" && !Array.isArray(value);
}

function messageForUnknownError(e: unknown): string {
  let msg: string;

  if (e instanceof Error) {
    msg = e.message;
  } else if (typeof e === "string") {
    msg = e;
  } else {
    // I give up -- JSON base case
    msg = JSON.stringify(e);
  }

  return msg;
}

// can have a case here to see if it is deployment or a docker localhost
// this means that the API can be connected to without the connection being hard coded
// Load environment variables
dotenv.config();
const DEV_PYTHON_EXECUTOR_CONN_ENDPOINT = "python-executor:8002";
const DEPLOYMENT_PYTHON_EXECUTOR_CONN_ENDPOINT = process.env.PYTHON_EXECUTOR_CONN_ENDPOINT;
const PYTHON_EXECUTOR_CONN_ENDPOINT = DEPLOYMENT_PYTHON_EXECUTOR_CONN_ENDPOINT || DEV_PYTHON_EXECUTOR_CONN_ENDPOINT;

export async function pythonExecutor(script: string, data: string): Promise<PythonExecutorResponse> {

  // Construct request body with the two files
  const formData = new FormData();

  formData.append("script.py", new Blob([script], { type: "text/x-python" }), "script.py");
  formData.append("data.csv", new Blob([data], { type: "text/csv" }), "data.csv");

  const base = `http://${PYTHON_EXECUTOR_CONN_ENDPOINT}`;
  const url = new URL("/execute", base);

  // Make POST request
  let response: Response;
  try {
    response = await fetch(url, {
      method: "POST",
      body: formData,
    });
  }
  catch (e) {
    return {
      error: `An error occurred while making the request to ${url.toString()} :(\n` +
        "Please inform a server admin.\n\n" + messageForUnknownError(e),
    } as PythonExecutorResponse;
  }

  if (response.body === null) {
    return {
      error: "No response body from the python_executor server :(\nPlease inform a server admin.",
    } as PythonExecutorResponse;
  }

  const contentType = response.headers.get("content-type")
  if (contentType !== null && contentType !== undefined && !contentType.startsWith("application/json")) {
    try {
      return {
        stdout: await response.text(),
        error: `Unexpected response content type ${contentType} from the python_executor server :(\n` +
          "Please inform a server admin.",
      } as PythonExecutorResponse;
    }
    catch (e) {
      return {
        error: `Response from python_executor in unexpected format ${contentType} also failed to be read :(\n` +
          "Please inform a server admin.\n\n" + messageForUnknownError(e),
      }
    }
  }

  let bodyRaw: unknown;
  try {
    bodyRaw = await response.json();
  }
  catch (e) {
    return {
      error: "An error occurred while parsing the response from the python_executor server :(\n" +
        "Please inform a server admin.\n\n" + messageForUnknownError(e),
    }
  }

  if (typeof bodyRaw === "string" && !response.ok) {
    return {
      error: bodyRaw,
    }
  }

  if (typeof bodyRaw !== "object" || !isObject(bodyRaw) || bodyRaw === null || bodyRaw === undefined) {
    return {
      error: "Unexpected response format from the python_executor server :(\nPlease inform a server admin.",
    }
  }

  const body = bodyRaw as Partial<PythonExecutorResponse>;

  if (Object.hasOwn(body, "data") && !Array.isArray(body.data!)) {
    return {
      error: "Unexpected data format received from the python_executor server :(\nPlease inform a server admin.",
    }
  }

  // I am very scared of crashing the server, so I have used Object.hasOwn everywhere
  return {
    data: Object.hasOwn(body, "data") ? body.data : undefined,
    error: Object.hasOwn(body, "error") ? body.error : undefined,
    stdout: Object.hasOwn(body, "stdout") ? body.stdout : undefined,
    stderr: Object.hasOwn(body, "stderr") ? body.stderr : undefined,
  }
}

export async function pythonExecutorScaling(script: string, data: string) : Promise<PythonExecutorScalingResponse> {
  // Validate given csv
  const csvLines = data.split('\n').filter(line => line.trim().length > 0);
  if (csvLines.length < 2) {
    return {
      error: "Uploaded csv has less than 2 lines.",
    }
  }

  // Start fetching all alias configs for current user at the same time as making request to python executor
  const aliasConfigPromise = Meteor
    .callAsync("aliasConfigs.getAllForOwner")
    .catch(() => null) as Promise<{ aliases: StudentAlias[] }[] | null>; // should never be null
  const response = await pythonExecutor(script, data);

  if (response.data === undefined || response.data.length === 0)
    return response as PythonExecutorScalingResponse;

  // Use the first column of csv data as the identifier, for whenever the python executor returns an index
  const csvFirstCol = csvLines
    .slice(1)
    .map(row => row.split(',')[0]);

  const aliasConfig = await aliasConfigPromise;

  // Build a lookup map from officialName -> alias object
  const aliasMap = new Map<string, StudentAlias>(
    (aliasConfig?.[0]?.aliases ?? []).map((a: StudentAlias) => [a.officialName, a])
  );
  // Build a lookup map from alias object -> officialName
  const officialNameMap = new Map<string, string>(
    (aliasConfig?.[0]?.aliases ?? [])
      .flatMap((a: StudentAlias) => [
        ...a.emails.map(email => [email, a.officialName]),
        ...a.gitUsernames.map(username => [username, a.officialName]),
      ]) as [string, string][]
  );

  // Turn response data into usable UserScalingSummary objects
  const filteredData = response.data
    .filter((row: unknown) => {
      // Ensure entries are arrays
      if (row === null || !Array.isArray(row))
        return false;
      // Ensure entries are of the right size
      if (row.length !== 2)
        return false;

      // Only accept arrays of [number, number] or [string, number]
      return !((typeof row[0] !== "string" || typeof row[1] !== "number") &&
        (typeof row[0] !== "number" || typeof row[1] !== "number"));
    })
    .map(row => row as [number | string, number])
    .map((row: [number | string, number]) => (
      [
        (typeof row[0] === "number") ? csvFirstCol[row[0]] : (row[0] as string),
        row[1]
      ] as [string, number]
    ))
    // Apply alias
    .map((row) => (
      [
        officialNameMap.get(row[0]) ?? row[0],
        row[1]
      ] as [string, number]
    ));

  function aliasToEmails(alias: StudentAlias): AliasEmail[]  {
    return [
      ...alias.emails.map(email => ({ username: alias.officialName, email })),
      ...alias.gitUsernames.map(username => ({ username, email: null })),
    ];
  }

  const scalingData = filteredData
    .map((row) => {
      const alias = aliasMap.get(row[0]);

      return {
        name: row[0],
        scale: row[1],
        aliases: alias ? aliasToEmails(alias) : [],
        finalGrade: null
      } as UserScalingSummary;
    })

  // Build final results
  return {
    ...response,
    data: scalingData,
  }
}