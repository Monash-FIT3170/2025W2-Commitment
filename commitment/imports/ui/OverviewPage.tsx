import React from "react";
import { NavBar } from "./components/landing-page/NavBar";
import InfoButton from "./components/ui/infoButton";
import { DateRangePicker } from "./components/ui/datePicker";

const overViewPageDescription = "This page gives an overview of the overall metrics of the top ."

export const OverviewPage = () => (
  <div className="m-0 scroll-smooth">
    <div className="flex flex-col gap-32">
        {/* Finn's Navbar goes here */}
        <NavBar /> 

        <div className="max-w-[1600px] mx-20 rounded-2xl bg-white p-8">

            <div className="flex flex-wrap items-center gap-x-[15rem] gap-y-4">
                <div className="flex items-center space-x-2">
                    <h1 className="text-5xl text-gray-900 font-robotoFlex">Overview</h1>
                    <InfoButton description={overViewPageDescription} />
                </div>

                <div className="flex flex-wrap gap-x-4 gap-y-2 items-start">
                    <div className="flex flex-col">
                        <label className="text-sm text-gray-600">Date Range*</label>
                        <DateRangePicker />
                    </div>
                </div>
            </div>

            <div className="mt-2 h-[2px] bg-black w-full sm:w-1/4" />

            <div className="mt-16">
                <p className="text-gray-700">This div is for Arosh</p>
            </div>
        </div>
    </div>
  </div>
);
