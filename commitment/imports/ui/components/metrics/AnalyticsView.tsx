import React, { useState, useEffect, useRef } from "react";
import { DateRange } from "react-day-picker";
import { useLocation } from "react-router-dom";
import { subWeeks } from "date-fns";
import InfoButton from "@base/infoButton";
import { DatePicker } from "./date-range-picker";
import BranchDropdownMenu from "./BranchDropdownMenu";
// import { dark2 } from "./colors";
import { ContributorDropdownMenu } from "./ContributorDropdownMenu";
import { HighlightCardWithGraph } from "./HighlightCard";
import { LeaderboardGraph } from "./LeaderboardGraph";
import { ContributionPieChart } from "./PieChartGraph";
import HeatmapGraph from "./HeatMapGraph";
import { useToast } from "@hook/useToast";
import { Subject } from "rxjs";

import { updateRepo } from "@api/call_repo";
import PercentileGraph from "./PercentileGraph";

import { AnalyticsData, MetricType, metricNames } from "@api/types";
import MetricDropdownMenu from "./MetricDropdownMenu";

// Main graph state
type MainGraphType = "heatmap" | "percentile";

// -----------------------------
// Main Component
// -----------------------------
export function AnalyticsView(): React.JSX.Element {
  const location = useLocation();
  const repoUrl: string | null =
    location.state?.repoUrl ?? localStorage.getItem("lastRepoUrl");
  const metricsPageDescription =
    "This page gives an overview of key metrics and performance trends.";

  const [mainGraph, setMainGraph] = useState<MainGraphType>("percentile");
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

  const { toast } = useToast();

  const handleToast = (msg: string) => {
    // update toast msg with a timeout of 1s
    toast({
      title: "Repository Status",
      description: msg,
      variant: "default",
    });
  };

  const msgHandlerRef = useRef(new Subject<string>());
  const updatedRef = useRef(new Subject<boolean>());

  const [filtersChanged, setFiltersChanged] = useState(false);

  useEffect(() => {
    const toastSub = msgHandlerRef.current.subscribe(handleToast);
    const updatedSub = updatedRef.current.subscribe((updated: boolean) => {
      if (!updated) {
        handleToast("Repo is out of sync, updating...");
      }
    });

    return () => {
      toastSub.unsubscribe();
      updatedSub.unsubscribe();
    };
  }, []);

  // Initial fetch only once
  useEffect(() => {
    if (!repoUrl) return;

    // Store the repository URL for navigation back to metrics
    // This only happens when user is actively viewing metrics
    localStorage.setItem("lastRepoUrl", repoUrl);

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
          setSelectedContributors(data.metadata.contributors); // default to all contributors
          setSelectedBranch(data.selections.selectedBranch);
          setDateRange(data.selections.selectedDateRange);
        }
        setLoading(false);
      }
    );
  }, []); // only runs once on mount

  const fetchAnalyticsDataMeteorCall = () => {
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
      }
    );
  };

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
  //   useEffect(() => {
  //     //REPLACE THIS WITH A SUBMIT BUTTON
  //     fetchAnalyticsData();
  //   }, [fetchAnalyticsData]);

  useEffect(() => {
    const navEntries = performance.getEntriesByType(
      "navigation"
    ) as PerformanceNavigationTiming[];
    const isReload = navEntries.length > 0 && navEntries[0].type === "reload";

    if (!isReload || !repoUrl) return;

    const interval = setInterval(() => {
      const status = Meteor.status();
      if (status.connected) {
        clearInterval(interval);
        updateRepo(repoUrl, updatedRef.current, msgHandlerRef.current)
          .then((proceed: boolean) => {
            if (proceed) {
              fetchAnalyticsData();
            }
          })
          .catch((e: Error) => {
            handleToast(`Update failed: ${e.message}`);
          });
      }
    }, 300);

    return () => clearInterval(interval);
  }, []);

  // Loading & Error States
  if (loading) return <div>Loading repo data...</div>;
  if (error) return <div>Error: {error}</div>;
  if (!analytics) return <div>No repo data available</div>;

  return (
    <div className="w-full h-full m-0 scroll-smooth border-t border-git-stroke-primary/40 bg-git-bg-elevated dark:bg-git-bg-primary">
      <div className="flex flex-col gap-32">
        <div className="w-full px-4 sm:px-6 md:px-8 lg:px-12 xl:px-20 py-4  bg-git-bg-elevated dark:bg-git-bg-primary">
          {/* Header */}
          <div className="mb-6 flex justify-between">
            <div className="flex flex-col pr-20">
              <div className="flex items-center gap-4 ">
                <h1 className="text-3xl text-foreground font-robotoFlex mt-4">
                  Metrics
                </h1>
                <InfoButton description={metricsPageDescription} />
              </div>
              <div className="h-[2px] bg-git-stroke-primary w-full mt-2" />
            </div>
            {/* Filters */}
            <div className="flex flex-wrap gap-x-8 gap-y-2 pt-4">
              <div className="flex flex-col">
                <p className="text-sm text-git-text-secondary">Date Range*</p>
                <DatePicker
                  defaultValue={dateRange}
                  onChange={(range: DateRange | undefined) => {
                    if (range) {
                      setDateRange(range);
                      setFiltersChanged(true);
                    }
                  }}
                />
              </div>
              <div className="flex flex-col">
                <div className="text-sm text-git-text-secondary">Branch*</div>
                <BranchDropdownMenu
                  branches={analytics.metadata.branches}
                  selected={selectedBranch}
                  onChange={(branch) => {
                    setSelectedBranch(branch);
                    setFiltersChanged(true);
                  }}
                />
              </div>
              <div className="flex flex-col">
                <div className="text-sm text-git-text-secondary">
                  Contributors*
                </div>
                <ContributorDropdownMenu
                  contributors={analytics.metadata.contributors}
                  selected={selectedContributors}
                  onChange={(contributors) => {
                    setSelectedContributors(contributors);
                    setFiltersChanged(true);
                  }}
                />
              </div>
              <div className="flex flex-col">
                <div className="text-sm text-git-text-secondary">Metrics*</div>
                <MetricDropdownMenu
                  metrics={metricNames}
                  selected={selectedMetrics}
                  onChange={(value: string) =>
                    setSelectedMetrics(value as MetricType)
                  }
                />
              </div>
              <div className="flex flex-col justify-end">
                <button
                  onClick={fetchAnalyticsData}
                  className="bg-git-int-primary text-git-int-text font-medium px-4 py-2 rounded-md shadow-sm hover:bg-git-int-primary-hover transition-all"
                >
                  Submit
                </button>
              </div>
            </div>
          </div>

          <div className="grid grid-cols-3 gap-5">
            {/* Highlight Cards */}
            <div className="flex flex-col gap-5 mb-12 col-start-3 row-end-auto">
              <div className="grid grid-cols-1 2xl:grid-cols-2 gap-5">
                <HighlightCardWithGraph
                  title="Total Commits"
                  value={analytics.metrics.highlights.totalCommits.total}
                  percentageChange={
                    analytics.metrics.highlights.totalCommits.percentageChange
                  }
                  isPositive={
                    analytics.metrics.highlights.totalCommits.isPositive
                  }
                  data={analytics.metrics.highlights.totalCommits.data}
                />
                {/* <HighlightCardWithGraph
                  title="Number of Branches"
                  value={analytics.metrics.highlights.numBranches}
                /> */}
                <HighlightCardWithGraph
                  title="Total Lines of Code"
                  value={analytics.metrics.highlights.totalLinesOfCode.total}
                  percentageChange={
                    analytics.metrics.highlights.totalLinesOfCode
                      .percentageChange
                  }
                  isPositive={
                    analytics.metrics.highlights.totalLinesOfCode.isPositive
                  }
                  data={analytics.metrics.highlights.totalLinesOfCode.data}
                />
              </div>
              {/* <HighlightCardWithGraph
                title="Number of Contributors"
                value={analytics.metrics.highlights.numContributors}
              /> */}

              {/* <div className="w-full min-h-[300px] h-full ">
                <ContributorLineGraph
                  data={analytics.metrics.contributors.lineGraph.data}
                  title={analytics.metrics.contributors.lineGraph.title}
                  xAxisLabel={
                    analytics.metrics.contributors.lineGraph.xAxisLabel
                  }
                  yAxisLabel={
                    analytics.metrics.contributors.lineGraph.yAxisLabel
                  }
                />
              </div> */}
              <div className="w-full min-h-[300px] h-full ">
                <ContributionPieChart
                  data={analytics.metrics.contributors.pieChart.data}
                  title={analytics.metrics.contributors.pieChart.title}
                  xAxisLabel={
                    analytics.metrics.contributors.leaderboard.xAxisLabel
                  }
                />
              </div>
            </div>

            <div className="flex flex-col col-span-2 row-start-1 gap-5">
              {/* Graphs */}
              {mainGraph === "percentile" ? (
                <PercentileGraph
                  data={analytics.metrics.contributors.scalingDistribution.data}
                  title={
                    analytics.metrics.contributors.scalingDistribution.title
                  }
                  setGraphType={setMainGraph}
                />
              ) : (
                <HeatmapGraph
                  data={analytics.metrics.contributors.heatMap.data}
                  title={analytics.metrics.contributors.heatMap.title}
                  setGraphType={setMainGraph}
                />
              )}

              {/* <div className="w-full min-h-[300px] h-full ">
                <LeaderboardGraph
                  data={analytics.metrics.contributors.leaderboard.data}
                  title={analytics.metrics.contributors.leaderboard.title}
                  xAxisLabel={
                    analytics.metrics.contributors.leaderboard.xAxisLabel
                  }
                />
              </div> */}
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}
