import { Meteor } from "meteor/meteor";
import { Subject } from "rxjs";
import * as fs from "fs";
import * as path from "path";

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

    if (result <= largestSize) return true;

    return false;
  },

  /**
   * Get the API README file content
   * @returns Promise<string> The README content as a string
   */
  async "api.getReadme"(): Promise<string> {
    // Define possible paths for the README file based on deployment environment
    const possiblePaths = [
      // Docker/production paths - configured in docker-compose.yml
      '/projects/api/README.md',
      '/app/api/README.md',
      // Local development paths - try to find project root dynamically
      ...(() => {
        // For local development, try to find the actual project root
        const currentDir = process.cwd();
        const projectRoot = path.resolve(currentDir, '../../../../..'); // Go up from build/programs/server
        return [
          path.join(projectRoot, 'api', 'README.md'),
          path.resolve(projectRoot, '../api/README.md'), // Alternative structure
        ];
      })(),
    ];
    
    // Attempt to locate and read the README file
    for (const readmePath of possiblePaths) {
      if (fs.existsSync(readmePath)) {
        return fs.readFileSync(readmePath, 'utf8');
      }
    }
    
    // Return informative error message if README cannot be located
    return `# API Documentation

## Error: README File Not Found

The API README file could not be located. Please ensure the Docker configuration properly mounts the \`api\` folder to \`/projects/api\` in the container.

---
*This is a fallback message. The actual README file could not be loaded.*`;
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
