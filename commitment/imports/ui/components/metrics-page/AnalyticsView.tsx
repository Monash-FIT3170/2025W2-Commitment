import React, { useState, useEffect } from "react";
import { DateRange } from "react-day-picker";
import { useLocation } from "react-router-dom";
import InfoButton from "../ui/infoButton";
import { DateRangePicker } from "./DatePickerButton";
import BranchDropdownMenu from "./BranchDropdownMenu";
import { ContributorDropdownMenu } from "./ContributorDropdownMenu";
import { HighlightCardWithGraph } from "./HighlightCard";
import { ContributorLineGraph } from "./LineGraph";
import { LeaderboardGraph } from "./LeaderboardGraph";
import { ContributionPieChart } from "./PieChartGraph";
// import GraphCard from "./GraphCard";
import HeatmapGraph from "./HeatMapGraph";

<<<<<<< HEAD
import {
  RepositoryData,
  ContributionEntry,
  FilteredData,
} from "/imports/api/types";
import { deserializeRepoData } from "/imports/api/serialisation";

// -----------------------------
// Mock Data
// -----------------------------
const dummyUsers = [
  "Alice",
  "Bob",
  "Charlie",
  "David",
  "Eva",
  "Frank",
  "Grace",
  "Helen",
];

const mockTotalLocData = [
  { value: 95 },
  { value: 90 },
  { value: 88 },
  { value: 85 },
  { value: 80 },
  { value: 78 },
  { value: 75 },
  { value: 72 },
  { value: 70 },
  { value: 65 },
  { value: 60 },
  { value: 58 },
  { value: 55 },
];

export const mockContributorDataset = {
  title: "Total Lines of Code",
  data: [
    { date: "2024-01-01", Alice: 120, Bob: 90, Charlie: 100 },
    { date: "2024-01-02", Alice: 140, Bob: 95, Charlie: 105 },
    { date: "2024-01-03", Alice: 135, Bob: 100, Charlie: 98 },
    { date: "2024-01-04", Alice: 160, Bob: 110, Charlie: 110 },
    { date: "2024-01-05", Alice: 170, Bob: 120, Charlie: 115 },
    { date: "2024-01-06", Alice: 180, Bob: 125, Charlie: 120 },
    { date: "2024-01-07", Alice: 190, Bob: 130, Charlie: 125 },
  ],
};

export const pieData = [
  { date: "2024-01-01", name: "Alice", count: 30 },
  { date: "2024-01-01", name: "Bob", count: 15 },
  { date: "2024-01-01", name: "Charlie", count: 22 },
  { date: "2024-01-01", name: "David", count: 18 },
  { date: "2024-01-02", name: "Alice", count: 22 },
  { date: "2024-01-02", name: "Bob", count: 18 },
  { date: "2024-01-02", name: "Charlie", count: 25 },
  { date: "2024-01-02", name: "David", count: 10 },
  { date: "2024-01-03", name: "Alice", count: 10 },
  { date: "2024-01-03", name: "Bob", count: 25 },
  { date: "2024-01-03", name: "Charlie", count: 17 },
  { date: "2024-01-03", name: "David", count: 20 },
  { date: "2024-01-04", name: "Alice", count: 17 },
  { date: "2024-01-04", name: "Bob", count: 20 },
  { date: "2024-01-04", name: "Charlie", count: 30 },
  { date: "2024-01-04", name: "David", count: 15 },
  { date: "2024-01-01", name: "Eva", count: 12 },
  { date: "2024-01-02", name: "Eva", count: 19 },
  { date: "2024-01-03", name: "Eva", count: 23 },
  { date: "2024-01-04", name: "Eva", count: 16 },
];

const metricsPageDescription =
  "This page gives an overview of key metrics and performance trends.";

export const generateRandomContributions = (
  startDate: Date,
  endDate: Date,
  users = dummyUsers
) => {
  if (!endDate || !isValid(endDate)) {
    return [];
  }

  const data: ContributionEntry[] = [];
  const totalDays = Math.floor(
    (endDate.getTime() - startDate.getTime()) / (1000 * 60 * 60 * 24)
  );

  users.forEach((user) => {
    Array.from({ length: totalDays + 1 }).forEach((_, i) => {
      const currentDate = addDays(startDate, i);
      const contributed = Math.random() < 0.45;
      const count = contributed ? Math.floor(Math.random() ** 2 * 150 + 5) : 0;

      data.push({
        name: user,
        date: format(currentDate, "yyyy-MM-dd"),
        count,
      });
    });
  });

  return data;
};

// const transformToPieChartData = (data: ContributionEntry[]) => {
//   const userTotals = data.reduce<Record<string, number>>((acc, entry) => {
//     acc[entry.name] = (acc[entry.name] || 0) + entry.count;
//     return acc;
//   }, {});

//   return Object.entries(userTotals).map(([user, contributions], i) => ({
//     user,
//     contributions,
//     fill: dark2[i % dark2.length],
//   }));
// };
=======
import { AnalyticsData, MetricType, metricNames } from "/imports/api/types";
import MetricDropdownMenu from "./MetricDropdownMenu";
>>>>>>> main

// -----------------------------
// Main Component
// -----------------------------
export function AnalyticsView(): React.JSX.Element {
  const location = useLocation();
  const repoUrl: string | null = location.state?.repoUrl ?? null;
  const metricsPageDescription =
    "This page gives an overview of key metrics and performance trends.";

  // setting up filters
  const [analytics, setAnalyticsData] = useState<AnalyticsData | null>(null);
  const [dateRange, setDateRange] = useState<DateRange | undefined>(undefined);
  const [selectedBranch, setSelectedBranch] = useState<string | undefined>(
    undefined
  );
  const [selectedContributors, setSelectedContributors] = useState<string[]>(
    []
  );
  const [selectedMetrics, setSelectedMetrics] = useState<MetricType>(
    MetricType.TOTAL_COMMITS
  );

  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

<<<<<<< HEAD
  const defaultDaysBack = 1000;

  const fetchFilteredData = () => {
    console.log("Fetching filtered data for repoUrl:", repoUrl);

=======
  // Initial fetch only once
  useEffect(() => {
>>>>>>> main
    if (!repoUrl) return;

    Meteor.call(
      "repo.getAnalyticsData",
      {
        repoUrl,
        startDate: dateRange?.from,
        endDate: dateRange?.to,
        branch: selectedBranch,
        contributors: selectedContributors,
        metric: selectedMetrics,
      },
      (err: Error, data: AnalyticsData) => {
        if (err) {
          setError(err.message);
        } else {
<<<<<<< HEAD
          setRepoData(deserializeRepoData(filtered.repositoryData));

          const checker = deserializeRepoData(filtered.repositoryData);

          console.log("AFTER DESERIALIZE - checking whole thing", checker);
          console.log(
            "AFTER DESERIALIZE - checking a commit:",
            checker.allCommits
          );

          setDateRange({
            from: filtered.dateRange.start,
            to: filtered.dateRange.end,
          });
          setLoading(false);
=======
          setAnalyticsData(data);
          setSelectedContributors(data.selections.selectedContributors);
          setSelectedBranch(data.selections.selectedBranch);
          setDateRange(data.selections.selectedDateRange);
>>>>>>> main
        }
        setLoading(false);
      }
    );
  }, []); // only runs once on mount

  const fetchAnalyticsData = React.useCallback(() => {
    if (!repoUrl) return;
    Meteor.call(
      "repo.getAnalyticsData",
      {
        repoUrl,
        startDate: dateRange?.from,
        endDate: dateRange?.to,
        branch: selectedBranch,
        contributors: selectedContributors,
        metric: selectedMetrics,
      },
      (err: Error, data: AnalyticsData) => {
        if (err) {
          setError(err.message);
        } else {
          setAnalyticsData(data);
        }
        setLoading(false);
      }
    );
  }, [
    repoUrl,
    selectedBranch,
    selectedContributors,
    dateRange,
    selectedMetrics,
  ]);

  // Fetch when component mounts or filters change
  useEffect(() => {
    fetchAnalyticsData();
  }, [fetchAnalyticsData]);

  // Loading & Error States
  if (loading) return <div>Loading repo data...</div>;
  if (error) return <div>Error: {error}</div>;
  if (!analytics) return <div>No repo data available</div>;

  return (
    <div className="m-0 scroll-smooth">
      <div className="flex flex-col gap-32">
        <div className="max-w-[1600px] mx-20 rounded-2xl bg-white p-8">
          {/* Header */}
          <div className="mb-6">
            <div className="flex items-center gap-4">
              <h1 className="text-5xl text-gray-900 font-robotoFlex">
                Metrics
              </h1>
              <InfoButton description={metricsPageDescription} />
            </div>
            <div className="h-[2px] bg-black w-1/4 mt-2" />
          </div>

          {/* Filters */}
          <div className="flex flex-wrap gap-8 mb-12">
            <div className="flex flex-col">
              <p className="text-sm text-gray-600">Date Range*</p>
              <DateRangePicker
                defaultValue={dateRange}
                onChange={(range) => {
                  if (range) setDateRange(range);
                }}
              />
            </div>
            <div className="flex flex-col">
              <label className="text-sm text-gray-600">Branch*</label>
              <BranchDropdownMenu
                branches={analytics.metadata.branches}
                selected={selectedBranch}
                onChange={setSelectedBranch}
              />
            </div>
            <div className="flex flex-col">
              <label className="text-sm text-gray-600">Contributors*</label>
              <ContributorDropdownMenu
                contributors={analytics.metadata.contributors}
                selected={selectedContributors}
                onChange={setSelectedContributors}
              />
            </div>
            <div className="flex flex-col">
              <label className="text-sm text-gray-600">Metrics*</label>
              <MetricDropdownMenu
                metrics={metricNames}
                selected={selectedMetrics}
                onChange={(value: string) =>
                  setSelectedMetrics(value as MetricType)
                }
              />
            </div>
          </div>

          {/* Highlight Cards */}
          <div className="flex flex-wrap gap-6 flex-1 min-w-[320px]">
            <HighlightCardWithGraph
              title="Total Commits"
              value={analytics.metrics.highlights.totalCommits.total}
              percentageChange={
                analytics.metrics.highlights.totalCommits.percentageChange
              }
              isPositive={analytics.metrics.highlights.totalCommits.isPositive}
              data={analytics.metrics.highlights.totalCommits.data}
            />
            <HighlightCardWithGraph
              title="Total Lines of Code"
              value={analytics.metrics.highlights.totalLinesOfCode.total}
              percentageChange={
                analytics.metrics.highlights.totalLinesOfCode.percentageChange
              }
              isPositive={
                analytics.metrics.highlights.totalLinesOfCode.isPositive
              }
              data={analytics.metrics.highlights.totalLinesOfCode.data}
            />
            <HighlightCardWithGraph
              title="No. of Contributors"
              value={analytics.metrics.highlights.numContributors}
            />
            <HighlightCardWithGraph
              title="Number of branches"
              value={analytics.metrics.highlights.numBranches}
            />
          </div>

          {/* Graphs */}
          <div className="flex flex-wrap gap-6 mt-12 mb-12">
            <ContributorLineGraph
              data={analytics.metrics.contributors.lineGraph.data}
              title={analytics.metrics.contributors.lineGraph.title}
              xAxisLabel={analytics.metrics.contributors.lineGraph.xAxisLabel}
              yAxisLabel={analytics.metrics.contributors.lineGraph.yAxisLabel}
            />
            <div className="rounded-2xl basis-1/3 min-w-[320px]">
              <LeaderboardGraph
                data={analytics.metrics.contributors.leaderboard.data}
                title={analytics.metrics.contributors.leaderboard.title}
                xAxisLabel={analytics.metrics.contributors.leaderboard.xAxisLabel}
              />
            </div>
<<<<<<< HEAD
            <div className="rounded-2xl basis-1/3 min-w-[320px]">
              <ContributionPieChart
                data={pieData}
                title="Commit Distribution"
              />
            </div>
=======
            <HeatmapGraph
              data={analytics.metrics.contributors.heatMap.data}
              title={analytics.metrics.contributors.heatMap.title}
            />
>>>>>>> main
          </div>
        </div>
      </div>
    </div>
  );
}
