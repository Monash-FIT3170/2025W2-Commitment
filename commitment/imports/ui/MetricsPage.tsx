import React, { useState, useMemo } from "react";
import { format, subDays, addDays } from "date-fns";
import { DateRange } from "react-day-picker";

import { NavBar } from "./components/landing-page/NavBar";
import InfoButton from "./components/ui/infoButton";
import { DateRangePicker } from "./components/ui/datePicker";
import { BranchDropDownMenu } from "./components/ui/branchDropDownMenu";
import UserContributionHeatMap from "./components/ui/heatmap";

const metricsPageDescription =
  "This page gives an overview of key metrics and performance trends.";

// DUMMY DATA
const dummyUsers = ["Alice", "Bob", "Charlie", "David", "Eva", "Frank", "Grace", "Helen"];

import {GitHubContribPie, generateDummyGitHubData} from "./components/ui/pieChart";

const dummyBranches = [
  "main",
  "development",
  "feature/login-page",
  "bugfix/fix-chart",
  "release/v1.2",
];



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

const dummyData = generateDummyGitHubData(8);

// DUMMY DATA ENDS

export const MetricsPage = () => {
  const today = new Date();
  const lastWeek = subDays(today, 6); // Last 7 days including today

  const initialDateRange: DateRange = { from: lastWeek, to: today };
  const [dateRange, setDateRange] = useState<DateRange>(initialDateRange);

  const startDate = dateRange.from!;
  const endDate = dateRange.to!;

  const data = useMemo(() => generateRandomContributions(startDate, endDate), [startDate, endDate]);

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
            <div className="absolute -top-12 left-[55%]">
              <p className="text-sm mb-1 text-gray-600">Branch*</p>
              <BranchDropDownMenu branches={dummyBranches} />
            </div>
          </div>

          <div className="mt-16">
            <p className="text-gray-700">This div is for Arosh</p>

            {/* Implementation of HeatMap */}
            <UserContributionHeatMap
              data={data}   //from dummy data
              startDate={startDate} //from date picker
              endDate={endDate}     //from date picker
              maxUsersToShow={24} //We need to stop at some point
              title="Heat Map" //If we're representing more than a single metric of heatmap - may need description as input
            />
            <GitHubContribPie data={dummyData} />
          </div>
            
        </div>
      </div>
    </div>
  );
};
