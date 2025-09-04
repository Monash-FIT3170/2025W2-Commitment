import { FilteredData, LineGraphData } from "/imports/api/types";

/**
 * LINEGRAPH FUNCTIONS
 */

/**
 * Returns the total number of commits per day for the line graph.
 * @param data Filtered Repository Data
 * @returns Array of LineGraphData objects
 */
export function linegraphTotalCommits(data: FilteredData): LineGraphData[] {
  const repoData = data.repositoryData;

  // Gather all contributors
  const allContributors = new Set<string>();
  repoData.allCommits.forEach((commit) => {
    allContributors.add(commit.value.contributorName);
  });

  // Bucket commits by day per contributor
  const commitsByDay: Record<string, Record<string, number>> = {};
  repoData.allCommits.forEach((commit) => {
    const date = new Date(commit.value.timestamp).toISOString().split("T")[0];
    const contributor = commit.value.contributorName;

    if (!commitsByDay[date]) commitsByDay[date] = {};
    commitsByDay[date][contributor] =
      (commitsByDay[date][contributor] || 0) + 1;
  });

  // Sort dates
  const sortedDates = Object.keys(commitsByDay).sort();

  // Prepare cumulative counts per contributor
  const cumulative: Record<string, number> = {};
  allContributors.forEach((c) => (cumulative[c] = 0));

  // Build LineGraphData array
  const dataArray: LineGraphData[] = [];
  sortedDates.forEach((date) => {
    const dailyCommits = commitsByDay[date];

    // Update cumulative totals
    Object.keys(dailyCommits).forEach((user) => {
      cumulative[user] = (cumulative[user] ?? 0) + dailyCommits[user];
    });

    // Create entry for this date
    const entry: LineGraphData = { date };
    allContributors.forEach((user) => {
      entry[user] = cumulative[user];
    });

    dataArray.push(entry);
  });

  return dataArray;
}

/**
 *
 * @param data
 * @returns
 */
export function linegraphLOC(data: FilteredData): LineGraphData[] {
  const byDate = new Map<string, Record<string, number>>();
  const repoData = data.repositoryData;

  // Gather all contributors
  const allContributors = new Set<string>();
  repoData.allCommits.forEach((commit) => {
    allContributors.add(commit.value.contributorName);
  });

  // Collect daily LOC
  repoData.allCommits.forEach((commit) => {
    const user = commit.value.contributorName;
    const date = new Date(commit.value.timestamp).toISOString().split("T")[0];

    // LOC snapshot for this commit
    const locThisCommit = commit.value.fileData.reduce(
      (sum, fileChange) => sum + fileChange.file.contents.split("\n").length,
      0
    );
    if (!byDate.has(date)) byDate.set(date, {});
    const bucket = byDate.get(date)!;
    bucket[user] = (bucket[user] ?? 0) + locThisCommit;
  });

  // Sort dates
  const sortedDates = Array.from(byDate.keys()).sort((a, b) =>
    a.localeCompare(b)
  );

  // Cumulative tracker
  const cumulative: Record<string, number> = {};
  allContributors.forEach((c) => (cumulative[c] = 0));

  const dataArray: LineGraphData[] = [];

  sortedDates.forEach((date) => {
    const dailyLOC = byDate.get(date)!;

    // update cumulative totals
    Object.keys(dailyLOC).forEach((user) => {
      cumulative[user] = (cumulative[user] ?? 0) + dailyLOC[user];
    });

    // include *all* contributors, even if they didn’t commit today
    const entry: LineGraphData = { date };
    allContributors.forEach((user) => {
      entry[user] = cumulative[user];
    });

    dataArray.push(entry);
  });

  return dataArray;
}

/**
 * Calculates the Commits Per Day and formats into a line graph strucutre.
 * @param data The filtered Data passed.
 * @returns An array of LineGraphData objects representing commits per day.
 */
export function linegraphCommitsPerDay(data: FilteredData): LineGraphData[] {
  const { dateRange, repositoryData } = data;
  const { allCommits, contributors } = repositoryData;

  // get contributor names
  const contributorNames = contributors.map((c) => c.value.name);

  // Format date as YYYY-MM-DD
  const formatDate = (d: Date) => d.toISOString().split("T")[0];

  // build a map: date -> {contributorName -> count}
  const commitMap: Record<string, Record<string, number>> = {};

  for (const commitObj of allCommits) {
    const commit = commitObj.value;
    const commitDate = formatDate(new Date(commit.timestamp));

    if (
      new Date(commit.timestamp) < dateRange.start ||
      new Date(commit.timestamp) > dateRange.end
    ) {
      continue;
    }

    // Initialize commitMap for this date
    if (!commitMap[commitDate]) {
      commitMap[commitDate] = {};
    }
    if (!commitMap[commitDate][commit.contributorName]) {
      commitMap[commitDate][commit.contributorName] = 0;
    }
    commitMap[commitDate][commit.contributorName]++;
  }
  const result: LineGraphData[] = [];
  const current = new Date(dateRange.start);
  while (current <= dateRange.end) {
    const dateStr = formatDate(current);
    const entry: LineGraphData = { date: dateStr };

    for (const name of contributorNames) {
      entry[name] = commitMap[dateStr]?.[name] || 0;
    }
    result.push(entry);
    current.setDate(current.getDate() + 1);
  }

  return result;
}

/**
 *
 * @param data
 */
export function linegraphLOCPerCommit(data: FilteredData): LineGraphData[] {
  const byDate = new Map<
    string,
    { loc: Record<string, number>; commits: Record<string, number> }
  >();
  const repoData = data.repositoryData;

  // Gather all contributors
  const allContributors = new Set<string>();
  repoData.allCommits.forEach((commit) => {
    allContributors.add(commit.value.contributorName);
  });

  // Collect daily LOC + commit counts
  repoData.allCommits.forEach((commit) => {
    const user = commit.value.contributorName;
    const date = new Date(commit.value.timestamp).toISOString().split("T")[0];

    // LOC snapshot for this commit
    const locThisCommit = commit.value.fileData.reduce(
      (sum, fileChange) => sum + fileChange.file.contents.split("\n").length,
      0
    );

    if (!byDate.has(date)) {
      byDate.set(date, { loc: {}, commits: {} });
    }
    const bucket = byDate.get(date)!;

    bucket.loc[user] = (bucket.loc[user] ?? 0) + locThisCommit;
    bucket.commits[user] = (bucket.commits[user] ?? 0) + 1;
  });

  // Sort dates
  const sortedDates = Array.from(byDate.keys()).sort((a, b) =>
    a.localeCompare(b)
  );

  // Cumulative trackers
  const cumulativeLOC: Record<string, number> = {};
  const cumulativeCommits: Record<string, number> = {};
  allContributors.forEach((c) => {
    cumulativeLOC[c] = 0;
    cumulativeCommits[c] = 0;
  });

  const dataArray: LineGraphData[] = [];

  sortedDates.forEach((date) => {
    const daily = byDate.get(date)!;

    // update cumulative totals
    Object.keys(daily.loc).forEach((user) => {
      cumulativeLOC[user] += daily.loc[user];
      cumulativeCommits[user] += daily.commits[user];
    });

    // entry for this date = avg LOC Per Commit
    const entry: LineGraphData = { date };
    allContributors.forEach((user) => {
      const commits = cumulativeCommits[user];
      entry[user] = commits > 0 ? Math.round(cumulativeLOC[user] / commits) : 0;
    });

    dataArray.push(entry);
  });

  return dataArray;
}
