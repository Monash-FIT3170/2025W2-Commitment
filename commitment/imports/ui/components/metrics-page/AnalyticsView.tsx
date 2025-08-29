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
// import { ContributionPieChart } from "./PieChartGraph";
// import GraphCard from "./GraphCard";

import { AnalyticsData } from "/imports/api/types";
import { MetricDropdownMenu } from "./MetricDropdownMenu";


// TEST : 

const selectedMetrics = ["LOC", "Commits", "LOC/Commits", "Commits per Day", "Total Commits"];
const placeholder = 3; 
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
  const [selectedMetrics, setSelectedMetrics] = useState<string[]>([]);

  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  // Initial fetch only once
  useEffect(() => {
    if (!repoUrl) return;

    Meteor.call(
      "repo.getAnalyticsData",
      {
        repoUrl,
        startDate: dateRange?.from,
        endDate: dateRange?.to,
        branch: selectedBranch,
        contributors: selectedContributors,
      },
      (err: Error, data: AnalyticsData) => {
        if (err) {
          setError(err.message);
        } else {
          setAnalyticsData(data);
          setSelectedContributors(data.selections.selectedContributors);
          setSelectedBranch(data.selections.selectedBranch);
          setSelectedMetrics(data.selections.)
          setDateRange(data.selections.selectedDateRange);
        }
        setLoading(false);
      }
    );
  }, [repoUrl]); // only runs once on mount

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
  }, [repoUrl, selectedBranch, selectedContributors, dateRange]);

  // Fetch when component mounts or filters change
  useEffect(() => {
    fetchAnalyticsData();
  }, [fetchAnalyticsData]);

  // Loading & Error States
  if (loading) return <div>Loading repo data...</div>;
  if (error) return <div>Error: {error}</div>;
  if (!analytics) return <div>No repo data available</div>;

  console.log(analytics);

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
                metrics={analytics.metricNames}
                selected={selectedMetrics}
                onChange={setSelectedMetrics}
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
              data={analytics.metrics.contributors.lineGraph}
              title="LOC Changes Over Time"
              xAxisLabel="Date"
              yAxisLabel="Lines of Code Changed"
            />
            <div className="rounded-2xl basis-1/3 min-w-[320px]">
              <LeaderboardGraph
                data={analytics.metrics.contributors.leaderboard}
                title="Top Contributors"
                xAxisLabel="Commits"
              />
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}
