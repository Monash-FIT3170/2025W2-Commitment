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

// -----------------------------
// Main Component
// -----------------------------
export function AnalyticsView(): React.JSX.Element {
  const location = useLocation();
  const repoUrl: string | null = location.state?.repoUrl ?? null;
  const metricsPageDescription =
    "This page gives an overview of key metrics and performance trends.";

  // setting up filters
  const [metrics, setMetricsData] = useState<AnalyticsData | null>(null);
  const [dateRange, setDateRange] = useState<DateRange | undefined>(undefined);
  const [selectedBranch, setSelectedBranch] = useState<string | undefined>(
    undefined
  );
  const [selectedContributors, setSelectedContributors] = useState<string[]>(
    []
  );

  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  const fetchAnalyticsData = () => {
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
          setLoading(false);
        } else {
          setMetricsData(data);
          setLoading(false);
        }
      }
    );
  };

  // Fetch when component mounts or filters change
  useEffect(() => {
    fetchAnalyticsData();
  }, [repoUrl, selectedBranch, selectedContributors, dateRange]);

  // Loading & Error States
  if (loading) return <div>Loading repo data...</div>;
  if (error) return <div>Error: {error}</div>;
  if (!metrics) return <div>No repo data available</div>;

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
                branches={metrics.metadata.branches}
                selected={selectedBranch}
                onChange={setSelectedBranch}
              />
            </div>
            <div className="flex flex-col">
              <label className="text-sm text-gray-600">Contributors*</label>
              <ContributorDropdownMenu
                contributors={metrics.metadata.contributors}
                selected={selectedContributors}
                onChange={setSelectedContributors}
              />
            </div>
          </div>

          {/* Highlight Cards */}
          <div className="flex flex-wrap gap-6 flex-1 min-w-[320px]">
            <HighlightCardWithGraph
              title="Total Commits"
              value={metrics.highlights.totalCommits.total}
              percentageChange={
                metrics.highlights.totalCommits.percentageChange
              }
              isPositive={metrics.highlights.totalCommits.isPositive}
              data={metrics.highlights.totalCommits.data}
            />
            <HighlightCardWithGraph
              title="Total Lines of Code"
              value={metrics.highlights.totalLinesOfCode.total}
              percentageChange={
                metrics.highlights.totalLinesOfCode.percentageChange
              }
              isPositive={metrics.highlights.totalLinesOfCode.isPositive}
              data={metrics.highlights.totalLinesOfCode.data}
            />
            <HighlightCardWithGraph
              title="No. of Contributors"
              value={metrics.highlights.numContributors}
            />
            <HighlightCardWithGraph
              title="Number of branches"
              value={metrics.highlights.numBranches}
            />
          </div>

          {/* Graphs */}
          <div className="flex flex-wrap gap-6 mt-12 mb-12">
            <ContributorLineGraph
              data={metrics.contributors.lineGraph}
              title="LOC Changes Over Time"
              xAxisLabel="Date"
              yAxisLabel="Lines of Code Changed"
            />
            <div className="rounded-2xl basis-1/3 min-w-[320px]">
              <LeaderboardGraph
                data={metrics.contributors.leaderboard}
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
