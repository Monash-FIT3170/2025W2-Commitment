import { Meteor } from "meteor/meteor";
import { Subject } from "rxjs";
import * as fs from 'fs';
import * as path from 'path';

import { getFilteredRepoDataServer } from "./filter";
import { tryFromDatabaseSerialised } from "./api/caching";
import {
  SerializableRepoData,
  FilteredData,
  AnalyticsData,
  Metadata,
  MetricsData,
  Selections,
  AllMetricsData,
  MetricType,
} from "@api/types";

import { getAllGraphData, getAllMetricsFromData } from "./repo_metrics";
import { applyAliasMappingIfNeeded } from "./alias_mapping";
import { getScaledResults } from "./ScalingFunctions";
import { ScalingConfig } from "/imports/ui/components/scaling/ScalingConfigForm";
import { executeCommand, assertSuccess } from "./api/shell";
import { checkIfExists } from "./api/git_commands";
import { getNumberOfContributors } from "./helper_functions";

export async function getFilteredRepoData(
  repoUrl: string,
  startDate: Date,
  endDate: Date,
  branch: string,
  contributor: string[]
): Promise<FilteredData> {
  // Get full repository data from db
  const repo = await tryFromDatabaseSerialised(repoUrl, null);

  // Apply alias mapping if user has config
  const userId = Meteor.userId();
  const mappedRepo = await applyAliasMappingIfNeeded(repo, userId || "");

  // Apply filtering
  const filteredData = getFilteredRepoDataServer(
    repoUrl,
    startDate,
    endDate,
    mappedRepo,
    branch,
    contributor
  );
  return filteredData;
}

Meteor.methods({
  /**
   * Check if a repository exists and is accessible
   * @param repoUrl The repository URL to check
   * @returns Promise<boolean> True if repository exists and is accessible
   */
  async "repo.checkExists"(repoUrl: string): Promise<boolean> {
    return executeCommand("")(checkIfExists(repoUrl))
      .then(assertSuccess("Repository does not exist"))
      .then(() => true)
      .catch(() => false);
  },

  /**
   * Get filtered repository data from the server
   * @param params.daysBack Number of days to look back (default: 7)
   * @param params.branch Branch to filter (optional)
   * @param params.contributor Contributor to filter (optional)
   * @returns FilteredData structure
   */
  async "repo.getFilteredData"(a: {
    repoUrl: string; // pass the URl from the frontend
    startDate: Date;
    endDate: Date;
    branch?: string;
    contributor?: string[];
  }): Promise<FilteredData> {
    // TODO do type checks here if needed

    return getFilteredData({
      ...a,
      userId: this.userId !== null ? this.userId : undefined,
    });
  },

  async "repo.getMetadata"(repoUrl: string): Promise<Metadata> {
    // TODO do type checks here if needed
    return getMetaData(repoUrl, this.userId !== null ? this.userId : undefined);
  },

  async "repo.getAnalyticsData"(d: {
    repoUrl: string;
    startDate?: Date;
    endDate?: Date;
    branch?: string;
    contributors?: string[];
    metric: MetricType;
  }): Promise<AnalyticsData> {
    // TODO do type checks here if needed

    return getAnalyticsData({
      ...d,
      userId: this.userId !== null ? this.userId : undefined,
    });
  },

  /**
   * Get all metrics for a repository with alias mapping applied
   * @param param0
   * @returns
   */
  async "repo.getAllMetrics"({ repoUrl }: { repoUrl: string }): Promise<AllMetricsData> {
    // Get repository data and apply alias mapping
    const repo: SerializableRepoData = await tryFromDatabaseSerialised(repoUrl, null);

    const mappedRepo = await applyAliasMappingIfNeeded(repo, this.userId || "");

    // Use the mapped data for metrics calculation
    return getAllMetricsFromData(mappedRepo);
  },

  async getScalingResults(data: ScalingConfig, repoUrl: string) {
    return getScaledResults(await tryFromDatabaseSerialised(repoUrl, null), data, repoUrl);
  },

  async isSmallContributorGroup(repoUrl: string = "", largestSize: number = 4): Promise<boolean> {
    const n = new Subject<string>();

    const result = getNumberOfContributors(await tryFromDatabaseSerialised(repoUrl, n));

    console.log("result: ", result);

    if (result <= largestSize) return true;

    return false;
  },

  /**
   * Get the API README file content
   * @returns Promise<string> The README content as a string
   */
  async "api.getReadme"(): Promise<string> {
    try {
      // Try multiple possible paths for the API README file
      const possiblePaths = [
        // From Meteor build directory
        path.join(process.cwd(), '..', '..', '..', '..', '..', 'api', 'README.md'),
        // From project root (if we can find it)
        path.join(process.cwd(), '..', '..', '..', '..', '..', '..', 'api', 'README.md'),
        // Absolute path fallback
        '/Users/ishratkaur/Documents/GitHub/2025W2-Commitment/api/README.md'
      ];
      
      console.log('Current working directory:', process.cwd());
      console.log('Trying possible paths:');
      
      for (const apiReadmePath of possiblePaths) {
        console.log('Trying:', apiReadmePath);
        if (fs.existsSync(apiReadmePath)) {
          console.log('Found API README at:', apiReadmePath);
          const content = fs.readFileSync(apiReadmePath, 'utf8');
          console.log('Successfully read API README, content length:', content.length);
          return content;
        }
      }
      
      // Fallback: Return the API README content directly
      console.log('File not found, using embedded content as fallback');
      return `# Intro

Hello! The purpose of this document is to give you some context about how this API works.

It works like this:

HTTP/WebSocket requests are handled by a request catcher, and are managed automatically by the operating system's thread scheduler.

Your repository will be cloned to a local folder (only the git metadata) and a plethora of git log/show commands will retrieve metadata, parsing the command line outputs

The parsed information is logically coalated into types and is send back to you!

# How it works

# Calling the API from your application

You can call the API via HTTP or using Websockets to recieve a reactive message!

this gives you flexibility as to whether:
http: you don't want to manage any progress loading the repo and just want the data
ws: you want to display any loading progress that might give the user something to mull about while the data loads in the background

Both methods require you to send a payload of some sorts. This payload should be in standard format, where git can clone the url safely from the command line taking it as an argument:

https://github.com/<OWNER>/<REPOSITORY_NAME>

If your repo is public, you have nothing to worry about! It should work automatically.
However, if your repo is private, the request may time out and give you an error because git did not have permission to clone the repo. To fix this, ensure that you are providing an access key inside your payload, which will help git authentication to approve cloning the repo! The general format for such a payload is the following (using https as an example):

https://<YOUR_USERNAME>:<YOUR_PRIVATE_KEY>@github.com/<OWNER>/<REPOSITORY_NAME>

# How our code interfaces with the API

# HTTP

We can interface with the api using code like this:

fetch("http://" + API_CONN_ENDPOINT, {
method: "POST",
headers: { "Content-Type": "application/json" },
body: JSON.stringify({ url }),
}).then((response) => {
if (!response.ok) reject(\`Haskell API returned status \${response.status}\`);
response.json().then((d) => resolve(d.data));
})

Things to note:
the method should always be POST, otherwise the API will reject the request

# WebSockets

Websockets can get a little complicated, so bear with me.
For every new repository you want analytics on, you must create a new Socket each time:

const socket = new WebSocket("ws://" + API_CONN_ENDPOINT)

this ensures that reactive messages from other repositories do not get tangled, and requests are handled efficiently
you can setup the socket like this:

socket.onopen = () => {

    // notify that connection to the api was successful
    if (notifier !== null) notifier.next("Connected to the API!");

    // send data through socket
    socket.send(
        JSON.stringify({
        url,
    })
    );

};

When the socket opens, you can safely send the payload containing the url you want to your analytics on

The socket can respond like this:

socket.onmessage = (event: WebSocket.MessageEvent) => {

    try {

        const { data } = event;
        const parsed = JSON.parse(data);

        if (parsed.type === "text_update" && notifier !== null) notifier.next(parsed.data);
        else if (parsed.type === "error") {
          reject(parsed.message);
          socket.close();
        } else if (parsed.type === "value") {
          resolve(parsed.data);
          socket.close();
        }
    } catch (err) {
        reject(err);
        socket.close();
    }

};

You can see that the kind of data you are fetching can be three kinds:
parsed.type === "text_update": the data is a string which contains the most recent loading stages of your repository
parsed.type === "error": something went wrong inside the application
parsed.type === "value": the API has sent you your analytics! You can safely resolve the promise and parse away!

Make sure that you are also guarding for errors and are closing the socket after it is used up:

socket.onerror = (\_err: WebSocket.ErrorEvent) => {

    const s = "Encountered a Websocket Error";
    if (notifier !== null) notifier.next(s);
    reject(new Error(s));
    socket.close();

};

# Types

Using typescript as an example, you can use these types directly in your application:

PS: keep in mind that Map objects
are automatically converted to plain objects
in javascript/typescript
so you can use functions to transpose those types
to Map objects for convenience. They will work by
having key: value entries where the key will be a
parameter inside the object

This is the type that will be sent to you:
type RepositoryData = Readonly<{

    name: string;
    branches: BranchData[];
    allCommits: Map<string, CommitData>;
    contributors: Map<string, ContributorData>;

}>;

type BranchData = Readonly<{

    branchName: string;
    commitHashes: string[];

}>;

type CommitData = Readonly<{

    commitHash: string;
    commitTitle: string;
    contributorName: string;
    description: string;
    timestamp: string;
    fileData: FileChanges[];

}>;

type FileChanges = Readonly<{

    filepath: string;
    oldFilePath: string;
    char: ChangeType;
    likeness: number;
    newLines: number;
    deletedLines: number;
    diff: string[];

}>;

type ChangeType = "A" | "M" | "D" | "R" | "C";

type ContributorData = Readonly<{

    name: string;
    emails: string[];

}>;

# Final words

For any further queries on how to call from the API, leave comments on our github page and we will do our best to respond in a timely fashion regarding your requests/inquiries

We thank you as a team for reading to the end of this document!`;
    } catch (error) {
      console.error('Error reading API README:', error);
      throw new Meteor.Error('read-error', 'Failed to read API README file');
    }
  },
});

export const getFilteredData = async ({
  repoUrl,
  startDate,
  endDate,
  branch,
  contributor,
  userId,
}: {
  repoUrl: string; // pass the URl from the frontend
  startDate: Date;
  endDate: Date;
  branch?: string;
  contributor?: string[];
  userId?: string;
}) => {
  // Get full repository data from db (fetches from API if not updated or in the database)
  const repo: SerializableRepoData = await tryFromDatabaseSerialised(repoUrl, null);

  // Apply alias mapping if user has config
  const mappedRepo = await applyAliasMappingIfNeeded(repo, userId || "");

  // Apply filtering
  const filteredData = getFilteredRepoDataServer(
    repoUrl,
    startDate,
    endDate,
    mappedRepo,
    branch,
    contributor
  );
  return filteredData;
};

export const getMetaData = async (repoUrl: string, userId?: string): Promise<Metadata> => {
  // Get full repository data from db (without checking whether its up to date)
  const repo: SerializableRepoData = await tryFromDatabaseSerialised(repoUrl, null);

  // Apply alias mapping if user has config
  const mappedRepo = await applyAliasMappingIfNeeded(repo, userId || "");

  return {
    repoUrl,
    repoName: mappedRepo.name,
    branches: mappedRepo.branches.map((b) => b.branchName),
    contributors: mappedRepo.contributors.map((c) => c.key),
    dateRange: {
      from: new Date(
        Math.min(...mappedRepo.allCommits.map((c) => new Date(c.value.timestamp).getTime()))
      ),
      to: new Date(),
    },
  };
};

export const getAnalyticsData = async ({
  repoUrl,
  startDate,
  endDate,
  branch,
  contributors,
  metric,
  userId,
}: {
  repoUrl: string;
  startDate?: Date;
  endDate?: Date;
  branch?: string;
  contributors?: string[];
  metric: MetricType;
  userId?: string;
}): Promise<AnalyticsData> => {
  /**
   * Get Repo Metadata first (contributors, branches, date range) etc
   *
   * Then get the filtered data depending on the parameters that have been passed
   *
   * Run our metrics functions on the filtered data
   *
   * Return the full AnalyticsData structure
   */

  // Get project metadata
  const metadata: Metadata = await getMetaData(repoUrl, userId);

  const selections: Selections = {
    selectedBranch:
      branch ??
      (metadata.branches.includes("main")
        ? "main"
        : metadata.branches.includes("master")
        ? "master"
        : metadata.branches[0]),
    selectedContributors: contributors ?? metadata.contributors,
    selectedMetrics: metric,
    selectedDateRange: {
      from: startDate || metadata.dateRange.from,
      to: endDate || metadata.dateRange.to,
    },
  };

  const filteredRepo: FilteredData = await getFilteredData({
    repoUrl,
    startDate: selections.selectedDateRange.from!,
    endDate: selections.selectedDateRange.to!,
    branch: selections.selectedBranch,
    contributor: selections.selectedContributors,
  });

  const metricsData: MetricsData = await getAllGraphData(filteredRepo, metric);

  // NOW WE DO STUFF WITH THE FILTERED REPO TO GET the specific metric!!
  const returnData: AnalyticsData = {
    metadata,
    selections,
    metrics: metricsData,
  };

  return returnData;
};
