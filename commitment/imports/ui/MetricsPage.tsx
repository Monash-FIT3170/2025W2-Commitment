import React, { useState, useMemo } from "react";
import { format, subDays, addDays } from "date-fns";
import { DateRange } from "react-day-picker";

import { NavBar } from "./components/landing-page/NavBar";
import InfoButton from "./components/ui/infoButton";
import { DateRangePicker } from "./components/ui/datePicker";
import { BranchDropDownMenu } from "./components/ui/branchDropDownMenu";
import UserContributionHeatMap from "./components/ui/heatmap";
import { ContributionPieChart } from "./components/ui/pieChart";
import {dark2} from "./components/ui/colors";

const metricsPageDescription =
  "This page gives an overview of key metrics and performance trends.";

const dummyUsers = ["Alice", "Bob", "Charlie", "David", "Eva", "Frank", "Grace", "Helen"];

const dummyBranches = [
  "main",
  "development",
  "feature/login-page",
  "bugfix/fix-chart",
  "release/v1.2",
];

const graphBackgroundColour = "#E8E8DD";

export const generateRandomContributions = (
  startDate: Date,
  endDate: Date,
  users = dummyUsers
) => {
  const data = [];
  const totalDays = Math.floor((endDate.getTime() - startDate.getTime()) / (1000 * 60 * 60 * 24));

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

  const data = useMemo(() => generateRandomContributions(startDate, endDate), [startDate, endDate]);
  const pieChartData = useMemo(() => transformToPieChartData(data), [data]);

  return (
    <div className="m-0 scroll-smooth">
      <div className="flex flex-col gap-32">
        <NavBar />

        <div className="max-w-[1600px] mx-20 rounded-2xl bg-white p-8">
          <div className="flex items-center space-x-2 w-2/5">
            <h1 className="text-5xl text-gray-900 font-robotoFlex">Metrics</h1>
            <InfoButton description={metricsPageDescription} />
          </div>

          <div className="mt-2 h-[2px] bg-black w-1/4" />

          <div className="relative">
            <div className="absolute -top-12 left-[28%]">
              <p className="text-sm mb-1 text-gray-600">Date Range*</p>
              <DateRangePicker
                defaultValue={dateRange}
                onChange={(range) => {
                  if (range) setDateRange(range);
                }}
              />
            </div>

            <div className="absolute -top-12 left-[55%] ">
              <p className="text-sm mb-1 text-gray-600">Branch*</p>
              <BranchDropDownMenu branches={dummyBranches} />
            </div>
          </div>

          <div className="mt-16 flex flex-row flex-nowrap items-start gap-6">
            <div
              className="w-max outline outline-2 rounded-2xl p-2" 
              style={{ backgroundColor: graphBackgroundColour, borderColor: "#353531"}}
              
            >
              <UserContributionHeatMap
                data={data}
                startDate={startDate}
                endDate={endDate}
                maxUsersToShow={24}
                title="Heat Map"
              />
            </div>

            <div
              className="flex-shrink-0 outline outline-2 rounded-2xl p-2"
              style={{ backgroundColor: graphBackgroundColour, borderColor: "#353531"}}
            >
              <ContributionPieChart data={pieChartData} />
            </div>
          </div>
        </div>
      </div>
    </div>
  );
};
