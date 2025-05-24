import React from "react";
import { NavBar } from "./components/landing-page/NavBar";
import InfoButton from "./components/ui/infoButton";
import { DateRangePicker } from "./components/ui/datePicker";
import { BranchDropDownMenu } from "./components/ui/branchDropDownMenu";
import  {HighlightCardWithGraph}  from "./components/metrics-page/HighlightCard";
import { ContributorLineGraph } from "./components/metrics-page/LineGraph";


// !!!: Remove this dummy data upon integration with AT3's real data
const dummyBranches = [
  "main",
  "development",
  "feature/login-page",
  "bugfix/fix-chart",
  "release/v1.2"
]

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
  title: "Lines of Codes Changed Over Time",
  data: [
    { date: "2024-01-01", Alice: 120, Bob: 90, Charlie: 100 },
    { date: "2024-01-02", Alice: 140, Bob: 95, Charlie: 105 },
    { date: "2024-01-03", Alice: 135, Bob: 100, Charlie: 98 },
    { date: "2024-01-04", Alice: 160, Bob: 110, Charlie: 110 },
    { date: "2024-01-05", Alice: 170, Bob: 120, Charlie: 115 },
    { date: "2024-01-06", Alice: 180, Bob: 125, Charlie: 120 },
    { date: "2024-01-07", Alice: 190, Bob: 130, Charlie: 125 },
  ]
}
const metricsPageDescription = "This page gives an overview of key metrics and performance trends."

export const MetricsPage = () => (
  <div className="m-0 scroll-smooth">
    <div className="flex flex-col gap-32">
        {/* Finn's Navbar goes here */}
        <NavBar /> 

        <div className="max-w-[1600px] mx-20 rounded-2xl bg-white p-8">

            <div className="flex flex-wrap items-center gap-x-[15rem] gap-y-4">
                <div className="flex items-center space-x-2">
                    <h1 className="text-5xl text-gray-900 font-robotoFlex">Metrics</h1>
                    <InfoButton description={metricsPageDescription} />
                </div>

                <div className="flex flex-wrap gap-x-4 gap-y-2 items-start">
                    <div className="flex flex-col">
                        <label className="text-sm text-gray-600">Date Range*</label>
                        <DateRangePicker />
                    </div>
                    <div className="flex flex-col">
                        <label className="text-sm text-gray-600">Branch*</label>
                        <BranchDropDownMenu branches={dummyBranches} />
                    </div>
                </div>
            </div>

            <div className="mt-2 h-[2px] bg-black w-full sm:w-1/4" />

            <div className="mt-16">
                <p className="text-gray-700">Why this div for arosh tbh</p>
                <div className="flex flex-wrap gap-8 mt-8">
                    <HighlightCardWithGraph title="Total Commits" value={123} percentageChange={20} isPositive={true} data={mockCommitLineData}/>
                    <HighlightCardWithGraph title="Total Lines of Code" value={4567} percentageChange={20} isPositive={false} data={mockTotalLocData} />
                    <HighlightCardWithGraph title="No. of Contributors" value={5} />
                    <HighlightCardWithGraph title="Number of branches" value={5} />
                </div>
                <div className = "flex flex-wrap gap-8 mt-8">
                    <ContributorLineGraph data={mockContributorDataset.data} title={mockContributorDataset.title} xAxisLabel="Date" yAxisLabel="Lines of Code Changed" />
                </div>
            </div>
        </div>
    </div>
  </div>
);
