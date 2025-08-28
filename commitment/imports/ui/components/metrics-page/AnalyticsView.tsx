import React, { useState, useEffect } from "react";
import { format, addDays, isValid } from "date-fns";
import { DateRange } from "react-day-picker";
import { useLocation } from "react-router-dom";

import InfoButton from "../ui/infoButton";
import { DateRangePicker } from "./DatePickerButton";
import BranchDropdownMenu from "./BranchDropdownMenu";
import { dark2 } from "../ui/colors";
import { ContributorDropdownMenu } from "./ContributorDropdownMenu";
import { HighlightCardWithGraph } from "./HighlightCard";
import { ContributorLineGraph } from "./LineGraph";
import { LeaderboardGraph } from "./LeaderboardGraph";
import { ContributionPieChart } from "./PieChartGraph";
// import GraphCard from "./GraphCard";

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

const transformToPieChartData = (data: ContributionEntry[]) => {
  const userTotals = data.reduce<Record<string, number>>((acc, entry) => {
    acc[entry.name] = (acc[entry.name] || 0) + entry.count;
    return acc;
  }, {});

  return Object.entries(userTotals).map(([user, contributions], i) => ({
    user,
    contributions,
    fill: dark2[i % dark2.length],
  }));
};

// -----------------------------
// Main Component
// -----------------------------
export function AnalyticsView() {
  const location = useLocation();
  const repoUrl: string | null = location.state?.repoUrl ?? null;

  // setting up filters
  const [repoData, setRepoData] = useState<RepositoryData | null>(null);
  const [dateRange, setDateRange] = useState<DateRange | undefined>(undefined);
  const [selectedBranch, setSelectedBranch] = useState<string>("main");
  const [selectedContributors, setSelectedContributors] = useState<string[]>(
    []
  );

  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  const defaultDaysBack = 1000;

  const fetchFilteredData = () => {
    console.log("Fetching filtered data for repoUrl:", repoUrl);

    if (!repoUrl) return;

    setLoading(true);
    setError(null);

    Meteor.call(
      "repo.getFilteredData",
      {
        daysBack: defaultDaysBack,
        branch: selectedBranch,
        contributors: selectedContributors,
        repoUrl,
      },
      (err: Error, filtered: FilteredData) => {
        if (err) {
          setError(err.message);
          setLoading(false);
        } else {
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
        }
      }
    );
  };

  // Fetch when component mounts or filters change
  useEffect(() => {
    fetchFilteredData();
  }, [repoUrl, selectedBranch, selectedContributors]);

  // Loading & Error States
  if (loading) return <div>Loading repo data...</div>;
  if (error) return <div>Error: {error}</div>;
  if (!repoData) return <div>No repo data available</div>;

  // Example placeholders for UI
  const branchData = Array.from(repoData.branches.map((b) => b.branchName));
  const contributorData = Array.from(repoData.contributors.values()).map(
    (c) => c.name
  );
  const numBranches = branchData.length;
  const numContributors = contributorData.length;

  const contributorCommitData = [
    {
      name: "yeetus feleetus",
      commits: -1,
    },
  ]; //getAllContributorsCommits(repoData).data;

  const totalCommits = {
    total: 0,
    percentageChange: 1,
    isPositive: false,
    data: [
      {
        value: 0,
      },
    ],
  };
  //calculateTotalCommits(repoData);

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
              <BranchDropdownMenu branches={branchData} />
            </div>
            <div className="flex flex-col">
              <label className="text-sm text-gray-600">Contributors*</label>
              <ContributorDropdownMenu contributors={contributorData} />
            </div>
          </div>

          {/* Highlight Cards */}
          <div className="flex flex-wrap gap-6 flex-1 min-w-[320px]">
            <HighlightCardWithGraph
              title="Total Commits"
              value={totalCommits.total}
              percentageChange={totalCommits.percentageChange}
              isPositive={totalCommits.isPositive}
              data={totalCommits.data}
            />
            <HighlightCardWithGraph
              title="Total Lines of Code"
              value={4567}
              percentageChange={-12}
              isPositive={false}
              data={mockTotalLocData}
            />
            <HighlightCardWithGraph
              title="No. of Contributors"
              value={numContributors}
            />
            <HighlightCardWithGraph
              title="Number of branches"
              value={numBranches}
            />
          </div>

          {/* Graphs */}
          <div className="flex flex-wrap gap-6 mt-12 mb-12">
            <ContributorLineGraph
              data={mockContributorDataset.data}
              title={mockContributorDataset.title}
              xAxisLabel="Date"
              yAxisLabel="Lines of Code Changed"
            />
            <div className="rounded-2xl basis-1/3 min-w-[320px]">
              <LeaderboardGraph
                data={contributorCommitData}
                title="Top Contributors"
                xAxisLabel="Commits"
              />
            </div>
            <div className="rounded-2xl basis-1/3 min-w-[320px]">
              <ContributionPieChart
                data={contributorCommitData}
                title="Commit Distribution"
                xAxisLabel="Commits"
              />
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}
