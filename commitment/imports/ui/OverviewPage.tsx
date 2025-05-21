import React from "react";
import { NavBar } from "./components/landing-page/NavBar";
import InfoButton from "./components/ui/infoButton";
import { DateRangePicker } from "./components/ui/datePickerDemo";

export const OverviewPage = () => (
  <div className="m-0 scroll-smooth">
    <div className="flex flex-col gap-32">
        {/* Finn's Navbar goes here */}
        <NavBar /> 

        <div className="max-w-[1600px] mx-20 rounded-2xl bg-white p-8">

            <div className="flex items-center space-x-2 w-2/5">
                <h1 className="text-5xl text-gray-900 font-robotoFlex">Overview</h1>
                <InfoButton />
            </div>

            <div className="mt-2 h-[2px] bg-black w-1/4" />

            <div className="relative">
                <div className="absolute -top-12 left-[28%]">
                    <p className="text-sm mb-1 text-gray-600">Date Range:</p>
                    <DateRangePicker />
                </div>
            </div>

            <div className="mt-16">
                <p className="text-gray-700">This div is for Arosh</p>
            </div>
        </div>
    </div>
  </div>
);
