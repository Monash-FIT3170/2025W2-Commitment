import React, { useState, useEffect } from "react";
import { DateRange } from "react-day-picker";
import { useLocation } from "react-router-dom";
import InfoButton from "../ui/infoButton";
import { DatePicker } from "./date-range-picker";
import BranchDropdownMenu from "./BranchDropdownMenu";
// import { dark2 } from "../ui/colors";
import { ContributorDropdownMenu } from "./ContributorDropdownMenu";
import { HighlightCardWithGraph } from "./HighlightCard";
import { ContributorLineGraph } from "./LineGraph";
import { LeaderboardGraph } from "./LeaderboardGraph";
import { ContributionPieChart } from "./PieChartGraph";
import HeatmapGraph from "./HeatMapGraph";
import { subWeeks } from "date-fns";

import { AnalyticsData, MetricType, metricNames } from "/imports/api/types";
import MetricDropdownMenu from "./MetricDropdownMenu";

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
  // set default date range to last 12 weeks
  const [dateRange, setDateRange] = useState<DateRange | undefined>(() => {
    const to = new Date();
    const from = subWeeks(to, 12);
    return { from, to };
  });

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
        metric: selectedMetrics,
      },
      (err: Error, data: AnalyticsData) => {
        if (err) {
          setError(err.message);
        } else {
          setAnalyticsData(data);
          setSelectedContributors(data.selections.selectedContributors);
          setSelectedBranch(data.selections.selectedBranch);
          setDateRange(data.selections.selectedDateRange);
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
    <div className="w-screen m-0 scroll-smooth p-10">
      <div className="flex flex-col gap-32">
        <div className="w-full px-4 sm:px-6 md:px-8 lg:px-12 xl:px-20 py-8 rounded-2xl bg-git-bg-elevated outline-2 outline-git-bg-secondary">
          {/* Header */}
          <div className="mb-6">
            <div className="flex items-center gap-4">
              <h1 className="text-5xl text-foreground font-robotoFlex">
                Metrics
              </h1>
              <InfoButton description={metricsPageDescription} />
            </div>
            <div className="h-[2px] bg-git-stroke-primary w-1/4 mt-2" />
          </div>
          {/* Filters */}
          <div className="flex flex-wrap gap-8 mb-12">
            <div className="flex flex-col">
              <p className="text-sm text-git-text-secondary">Date Range*</p>
              <DatePicker
                defaultValue={dateRange}
                onChange={(range: DateRange | undefined) => {
                  if (range) setDateRange(range);
                }}
              />
            </div>
            <div className="flex flex-col">
              <div className="text-sm text-git-text-secondary">Branch*</div>
              <BranchDropdownMenu
                branches={analytics.metadata.branches}
                selected={selectedBranch}
                onChange={setSelectedBranch}
              />
            </div>
            <div className="flex flex-col">
              <div className="text-sm text-git-text-secondary">Contributors*</div>
              <ContributorDropdownMenu
                contributors={analytics.metadata.contributors}
                selected={selectedContributors}
                onChange={setSelectedContributors}
              />
            </div>
            <div className="flex flex-col">
              <label className="text-sm text-git-text-secondary">Metrics*</label>
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
          <div className="grid grid-cols-1 lg:grid-cols-2 3xl:grid-cols-4 gap-6 mb-12">
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
              title="Number of Branches"
              value={analytics.metrics.highlights.numBranches}
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
              title="Number of Contributors"
              value={analytics.metrics.highlights.numContributors}
            />
          </div>

          {/* Graphs */}
          <div className="grid grid-cols-1 xl:grid-cols-2 3xl:grid-cols-3 gap-6 w-full">
            <div className="w-full min-h-[300px] h-full xl:max-3xl:col-span-2">
              <LeaderboardGraph
                data={analytics.metrics.contributors.leaderboard.data}
                title={analytics.metrics.contributors.leaderboard.title}
                xAxisLabel={
                  analytics.metrics.contributors.leaderboard.xAxisLabel
                }
              />
            </div>
            <div className="w-full min-h-[300px] h-full ">
              <ContributorLineGraph
                data={analytics.metrics.contributors.lineGraph.data}
                title={analytics.metrics.contributors.lineGraph.title}
                xAxisLabel={analytics.metrics.contributors.lineGraph.xAxisLabel}
                yAxisLabel={analytics.metrics.contributors.lineGraph.yAxisLabel}
              />
            </div>
            <div className="w-full min-h-[300px] h-full">
              <ContributionPieChart
                data={analytics.metrics.contributors.pieChart.data}
                title={analytics.metrics.contributors.pieChart.title}
              />
            </div>
            <div className="w-full col-span-full">
              <HeatmapGraph
                data={analytics.metrics.contributors.heatMap.data}
                title={analytics.metrics.contributors.heatMap.title}
              />
            </div>
          </div>
        </div>
      </div>
    </div>
  );
} 
