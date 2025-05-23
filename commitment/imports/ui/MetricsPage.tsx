import React from "react";
import { NavBar } from "./components/landing-page/NavBar";
import InfoButton from "./components/ui/infoButton";
import { DateRangePicker } from "./components/ui/datePicker";
import { BranchDropDownMenu } from "./components/ui/branchDropDownMenu";
import  {HighlightCardWithGraph}  from "./components/metrics-page/HighlightCard";


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
const metricsPageDescription = "This page gives an overview of key metrics and performance trends."

export const MetricsPage = () => (
  <div className="m-0 scroll-smooth">
    <div className="flex flex-col gap-32">
        {/* Finn's Navbar goes here */}
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
                    <DateRangePicker />
                </div>
                <div className="absolute -top-12 left-[55%]">
                    <p className="text-sm mb-1 text-gray-600">Branch*</p>
                    <BranchDropDownMenu branches={dummyBranches}/>
                </div>
                
            </div>

            <div className="mt-16">
                <p className="text-gray-700">This div is for Arosh</p>
                <div className="flex flex-wrap gap-8 mt-8">
                    <HighlightCardWithGraph title="Total Commits" value={123} percentageChange={20} isPositive={true} data={mockCommitLineData}/>
                    <HighlightCardWithGraph title="Total Lines of Code" value={4567} percentageChange={20} isPositive={false} data={mockTotalLocData} />
                    <HighlightCardWithGraph title="No. of Contributors" value={5} />
                    <HighlightCardWithGraph title="Number of branches" value={5} />
                </div>
            </div>
        </div>
    </div>
  </div>
);
