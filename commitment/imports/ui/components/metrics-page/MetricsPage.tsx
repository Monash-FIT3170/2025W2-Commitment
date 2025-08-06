import React, { useState, useMemo } from "react";
import { format, subDays, addDays, isValid } from "date-fns";
import { DateRange } from "react-day-picker";
import InfoButton from "../ui/infoButton";
import { DateRangePicker } from "./DatePickerButton";
import { BranchDropDownMenu } from "./BranchDropDownMenuButton";
import UserContributionHeatMap from "./HeatMapGraph";
import { dark2 } from "../ui/colors";
import { ContributorDropDownMenu } from "./ContributorDropDownButton";
import { HighlightCardWithGraph } from "./HighlightCard";
import { ContributorLineGraph } from "./LineGraph";
import { LeaderboardGraph } from "./LeaderboardGraph";
import { ContributionPieChart } from "./PieChartGraph";
import { topContributors } from "../../lib/utils";
import GraphCard from "./GraphCard";
import { getRepoData, getCommitsMap, getContributorsMap } from "@ui/components/utils/repo_cache";

// !!!: Remove this dummy data upon integration with AT3's real data
const dummyBranches = [
  "main",
  "development",
  "feature/login-page",
  "bugfix/fix-chart",
  "release/v1.2",
];

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

const dummyContributors = [
  "Michael",
  "Andrew",
  "Jessica",
  "Andy",
  "Barry",
  "Georgia",
  "Mary",
  "Sophie",
  "Bill",
  "Simon",
  "George",
  "Tim",
  "Rachel",
  "Lora",
];

const mockLocLineData = [
  { value: 50 },
  { value: 58 },
  { value: 62 },
  { value: 65 },
  { value: 60 },
  { value: 68 },
  { value: 72 },
  { value: 70 },
  { value: 76 },
  { value: 85 },
  { value: 82 },
  { value: 90 },
  { value: 95 },
];
const mockCommitLineData = [
  { value: 50 },
  { value: 58 },
  { value: 62 },
  { value: 65 },
  { value: 60 },
  { value: 68 },
  { value: 72 },
  { value: 70 },
  { value: 76 },
  { value: 85 },
  { value: 82 },
  { value: 90 },
  { value: 95 },
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
  title: "Total Lines of Code Over Time",
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

export const mockAllContributorDataset = {
  title: "All Contributor Commits",
  data: [
    { name: "Alice", commits: 100 },
    { name: "Bob", commits: 80 },
    { name: "Michael", commits: 40 },
    { name: "Andrew", commits: 130 },
    { name: "David", commits: 60 },
    { name: "Tim", commits: 70 },
    { name: "George", commits: 95 },
  ],
};

const topUsers = topContributors(mockAllContributorDataset.data);

const metricsPageDescription =
  "This page gives an overview of key metrics and performance trends.";


export const generateRandomContributions = (
  startDate: Date,
  endDate: Date,
  users = dummyUsers
) => {
  if (!endDate || !isValid(endDate)) {
    // In the case where no end date is given
    return [];
  } 
  console.log(startDate, endDate, "both")
  const data = [];
  const totalDays = Math.floor(
    (endDate.getTime() - startDate.getTime()) / (1000 * 60 * 60 * 24)
  );

  for (const user of users) {
    for (let i = 0; i <= totalDays; i++) {
      const currentDate = addDays(startDate, i);
      const contributed = Math.random() < 0.45;
      const count = contributed
        ? Math.floor(Math.pow(Math.random(), 2) * 150 + 5)
        : 0;

      data.push({
        name: user,
        date: format(currentDate, "yyyy-MM-dd"),
        count,
      });
    }
  }

  return data;
};

const transformToPieChartData = (data: any[]) => {
  const userTotals: Record<string, number> = {};

  for (const entry of data) {
    userTotals[entry.name] = (userTotals[entry.name] || 0) + entry.count;
  }

  return Object.entries(userTotals).map(([user, contributions], i) => ({
    user,
    contributions,
    fill: dark2[i % dark2.length],
  }));
};

export const MetricsPage = () => {
  const today = new Date();
  const lastWeek = subDays(today, 6); // Last 7 days including today

  const initialDateRange: DateRange = { from: lastWeek, to: today };
  const [dateRange, setDateRange] = useState<DateRange>(initialDateRange);

  const startDate = dateRange.from!;
  const endDate = dateRange.to!;

  const data = useMemo(
    () => generateRandomContributions(startDate, endDate),
    [startDate, endDate]
  );
  const pieChartData = useMemo(() => transformToPieChartData(data), [data]);

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
              <label className="text-sm text-gray-600">Date Range*</label>
              <DateRangePicker
                defaultValue={dateRange}
                onChange={(range) => {
                  if (range) setDateRange(range);
                }}
              />
            </div>
            <div className="flex flex-col">
              <label className="text-sm text-gray-600">Branch*</label>
              <BranchDropDownMenu branches={dummyBranches} />
            </div>
            <div className="flex flex-col">
              <label className="text-sm text-gray-600">Contributors*</label>
              <ContributorDropDownMenu contributors={dummyContributors} />
            </div>
          </div>

          <div className="flex flex-wrap gap-6">
            {/* Heatmap */}
            {/* <div
              className="outline outline-2 rounded-2xl p-2 basis-1/3 min-w-[320px]"
              style={{
                backgroundColor: graphBackgroundColour,
                outlineColor: "#35353140",
              }}
            >
              <UserContributionHeatMap
                data={data}
                startDate={startDate}
                endDate={endDate}
                maxUsersToShow={24}
                title="Heat Map"
              />
            </div> */}
            <GraphCard>
              <UserContributionHeatMap
                data={data}
                startDate={startDate}
                endDate={endDate}
                maxUsersToShow={24}
                title="Heat Map"
              />
            </GraphCard>

            {/* Pie Chart */}
            {/* <div
              className="outline outline-2 rounded-2xl p-2 flex-1 min-w-[320px]"
              style={{
                backgroundColor: graphBackgroundColour,
                outlineColor: "#35353140",
              }}
            >
              <ContributionPieChart data={pieChartData} />
            </div> */}
            <GraphCard>
              <ContributionPieChart data={pieChartData} />
            </GraphCard>

            <div className="flex flex-wrap gap-6 flex-1 min-w-[320px]">
              <HighlightCardWithGraph
                title="Total Commits"
                value={123}
                percentageChange={20}
                isPositive={true}
                data={mockCommitLineData}
              />
              <HighlightCardWithGraph
                title="Total Lines of Code"
                value={4567}
                percentageChange={-12}
                isPositive={false}
                data={mockTotalLocData}
              />
              <HighlightCardWithGraph title="No. of Contributors" value={5} />
              <HighlightCardWithGraph title="Number of branches" value={5} />
            </div>
          </div>

          {/* Highlights & Graphs */}

          <div className="flex flex-wrap gap-6 mt-12 mb-12">
            {/* <div className="rounded-2xl p-2 basis-1/3 min-w-[320px]">

            </div> */}
            <ContributorLineGraph
              data={mockContributorDataset.data}
              title={mockContributorDataset.title}
              xAxisLabel="Date"
              yAxisLabel="Lines of Code Changed"
            />

            <div className="rounded-2xl p-2 basis-1/3 min-w-[320px]">
              <LeaderboardGraph
                data={topUsers}
                title="Top Contributors Based on All Commits"
                xAxisLabel="Commits"
              />
            </div>
          </div>
        </div>
      </div>
    </div>
  );
};
