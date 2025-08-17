import React, { useState, useMemo, useEffect } from "react";
import { format, subDays, addDays, isValid, set } from "date-fns";
import { DateRange } from "react-day-picker";
import InfoButton from "../ui/infoButton";
import { DateRangePicker } from "./DatePickerButton";
import { BranchDropdownMenu } from "./BranchDropdownMenu";
import UserContributionHeatMap from "./HeatMapGraph";
import { dark2 } from "../ui/colors";
import { ContributorDropdownMenu } from "./ContributorDropdownMenu";
import { HighlightCardWithGraph } from "./HighlightCard";
import { ContributorLineGraph } from "./LineGraph";
import { LeaderboardGraph } from "./LeaderboardGraph";
import { ContributionPieChart } from "./PieChartGraph";
import { topContributors } from "../../lib/utils";
import GraphCard from "./GraphCard";
import { useLocation } from "react-router-dom";
import { getContributors, getBranches, getAllContributorsCommits} from "/imports/ui/components/utils/metric_functions";
import { RepositoryData } from "/server/commitment_api/types";
import { Subject } from "rxjs";
import { get } from "http";
import { getFilteredRepoData } from "../utils/data_filter";



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
  console.log(startDate, endDate, "both");
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

export const AnalyticsView = () => {
  const today = new Date();
  const lastWeek = subDays(today, 6); // Last 7 days including today


  // // attempt to get the repo data from the location state
  //   // storing the repo data by calling getRepoData in metric_functions.tsx
  const location = useLocation();
  const repoUrl = location.state?.repoUrl 
  const [repoData, setRepoData]= useState<RepositoryData | null> (null) ;
  const [dateRange, setDateRange] = useState<DateRange | undefined>(undefined);
  const [loading, setLoading] = useState<boolean>(true);  // Add loading state
  const [error, setError] = React.useState<string | null>(null);
  const defaultDaysBack = 1000; 

  useEffect(() => {
    // check if entering this useEffect
    console.log("Entering useEffect with repoUrl:", repoUrl);
    if (!repoUrl) {
      setLoading(false);
      console.log("No repo URL found");
      return;
    }
    console.log("Starting getFilteredRepoData with URL:", repoUrl);

    const notifier = new Subject<string>();
    const filtered = getFilteredRepoData(repoUrl, notifier, defaultDaysBack, "main");

    filtered.repositoryData.then((data) => {
        console.log("Filtered repo data fetched:", data);
        setRepoData(data);
        setDateRange({ from: filtered.dateRange.start, to: filtered.dateRange.end });
        console.log("Date range being used:", filtered.dateRange);

        setLoading(false);
        notifier.complete();
      })
      .catch((e) => {
        setError(e.message);
        setLoading(false);
        notifier.complete();
      });
  }, [repoUrl]);

  //   // establishes date range defaults
  // const today = new Date();
  // const lastWeek = subDays(today, 6); // Last 7 days including today

  // const initialDateRange: DateRange = { from: lastWeek, to: today };
  // // const [dateRange, setDateRange] = useState<DateRange>(initialDateRange);

  // const startDate = dateRange.from!;
  // const endDate = dateRange.to!;

  // const data = useMemo(
  //   () => generateRandomContributions(startDate, endDate),
  //   [startDate, endDate]
  // );


  // const pieChartData = useMemo(() => transformToPieChartData(data), [data]);

  // checking if repo data is still loading
  if (loading) {
    return <div>Loading repo data...</div>;
  }
  // checking if error has occurred
  if (error) {
    return <div>Error: {error}</div>;
  }

  // checking if the repo data exists 
  if (!repoData) {
    return <div>No repo data available</div>;
  }

  // extracting information from metric functions 
  const contributorData = getContributors(repoData);
  const numContributors = contributorData.length;
  const branchData = getBranches(repoData);
  const numBranches = branchData.length;
  const contributorCommitData = getAllContributorsCommits(repoData).data;

  // const filteredRepoData = getFilteredRepoData(repoUrl, new Subject<string>());
  // console.log("here is the filtered data", filteredRepoData);


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
              <BranchDropDownMenu branches={branchData} />
            </div>
            <div className="flex flex-col">
              <label className="text-sm text-gray-600">Contributors*</label>
              <ContributorDropDownMenu contributors={contributorData} />
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
            {/* <GraphCard>
              <UserContributionHeatMap
                data={data}
                startDate={startDate}
                endDate={endDate}
                maxUsersToShow={24}
                title="Heat Map"
              />
            </GraphCard> */}

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
            {/* <GraphCard>
              <ContributionPieChart data={pieChartData} />
            </GraphCard> */}

            <UserContributionHeatMap
              data={data}
              startDate={startDate}
              endDate={endDate}
              maxUsersToShow={24}
              title="Heat Map"
            />

            {/* Pie Chart */}

            <ContributionPieChart data={pieChartData} />

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
              <HighlightCardWithGraph title="No. of Contributors" value={numContributors} />
              <HighlightCardWithGraph title="Number of branches" value={numBranches} />
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

            <div className="rounded-2xl basis-1/3 min-w-[320px]">
              <LeaderboardGraph
                data={contributorCommitData}
                title="Top Contributors"
                xAxisLabel="Commits"
              />
            </div>
          </div>
        </div>
      </div>
    </div>
  );
};
