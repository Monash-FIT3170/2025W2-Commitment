
import { Meteor } from "meteor/meteor";
import { Subject } from "rxjs";
import { check, Match } from "meteor/check";

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
import { getFilteredRepoDataServer } from "./filter";
import { tryFromDatabaseSerialised } from "./api/caching";

import { getAllGraphData, getAllMetricsFromData } from "./repo_metrics";
import { applyAliasMappingIfNeeded } from "./alias_mapping";
import { getScaledResults } from "./ScalingFunctions";
import { ScalingConfig } from "/imports/ui/components/scaling/ScalingConfigForm";
import { executeCommand, assertSuccess } from "./api/shell";
import { checkIfExists } from "./api/git_commands";
import { getNumberOfContributors } from "./helper_functions";
import { evaluateCommitMessageRules, RegexRule, ContributorRegexScore } from "./regex_scoring";

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

async "regex.evaluate"(params: {
  /**
   * Analyze commit messages by contributors
   * Score them according to a user's array of regular expressions, which are weighted and signed ('+' good weight, '-' bad weight)
   * Standardizes so that commit frequency does not skew the general score
   * Gives a scaling suggestion based on general score (same approach/scaling as scaling feature)
   */
  repoUrl: string;
  rules: Array<{
    regex: string;
    scale: number;
    sign: "+" | "-";
    key?: string;
    flags?: string;
  }>;
  branch?: string;
  startDate?: Date;
  endDate?: Date;
  contributors?: string[];
}): Promise<ContributorRegexScore[]> {

  // validate parameters
  check(params, {
    repoUrl: String,
    rules: [
      Match.ObjectIncluding({
        regex: String,
        scale: Number,
        sign: Match.OneOf("+", "-"),
        key: Match.Optional(String),
        flags: Match.Optional(String),
      }),
    ],
    branch: Match.Optional(String),
    startDate: Match.Optional(Date),
    endDate: Match.Optional(Date),
    contributors: Match.Optional([String]),
  });

  // destructure 
  const typedParams = params as {
    repoUrl: string;
    rules: Array<{
      regex: string;
      scale: number;
      sign: "+" | "-";
      key?: string;
      flags?: string;
    }>;
    branch?: string;
    startDate?: Date;
    endDate?: Date;
    contributors?: string[];
  };

  const { repoUrl, rules, branch, startDate, endDate, contributors } = typedParams;

  // load and alias map repo
  const repo: SerializableRepoData = await tryFromDatabaseSerialised(repoUrl, null);
  const mappedRepo = await applyAliasMappingIfNeeded(repo, this.userId || "");

  // branch naming selection (defaults to main/master/first branch)
  const branchNames = mappedRepo.branches.map((b) => b.branchName);
  const selectedBranch = (() => {
    if (branch) return branch;
    if (branchNames.includes("main")) return "main";
    if (branchNames.includes("master")) return "master";
    return branchNames[0];
  })();

  // time selection - defaults to full history (will double check if this is sound)
  const timestamps = mappedRepo.allCommits.map((c) =>
    new Date(c.value.timestamp).getTime()
  );
  const defaultFrom =
    startDate ??
    (timestamps.length > 0 ? new Date(Math.min(...timestamps)) : new Date(0));
  const defaultTo = endDate ?? new Date();

  // snapshot of repo with needed attributes
  const filteredRepo = getFilteredRepoDataServer(
    repoUrl,
    defaultFrom,
    defaultTo,
    mappedRepo,
    selectedBranch,
    contributors
  );

  // normalize to fit backend structure
  const internalRules: RegexRule[] = (rules ?? []).map((r) => ({
    pattern: r.regex,
    weight: r.scale,
    negate: r.sign === "-",
    key: r.key,
    flags: r.flags,
  }));

  return evaluateCommitMessageRules(filteredRepo.repositoryData, internalRules);
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
