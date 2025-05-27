import {
  Tabs,
  TabsList,
  TabsTrigger,
  TabsContent,
} from "@ui/components/ui/tabs";
import React from "react";
import { Settings } from "lucide-react";
import { OverviewPage } from "../../OverviewPage";
import { MetricsPage } from "../../MetricsPage";

interface TabData {
  value: string;
  label: string;
}

const allTabData: TabData[] = [
  {
    value: "overview",
    label: "Overview",
  },
  {
    value: "metrics",
    label: "Metrics",
  },
  {
    value: "scaling",
    label: "Scaling",
  },
];

export function MetricsTabs() {
  return (
    <div className="w-full bg-[#FEFEFA]">
      {/* Top bar */}
      <div className="flex items-center justify-between px-4 py-3 border-b border-gray-200 bg-[#FEFEFA]">
        <div className="flex items-center gap-3">
          <h2 className="text-lg font-semibold text-gray-800">Repo Name</h2>
          {/* Bookmark icon placeholder */}
          <div className="w-6 h-6 bg-gray-300 rounded" />{" "}
        </div>
        <Settings className="w-5 h-5 text-gray-500 hover:text-gray-700 cursor-pointer" />
      </div>

      <Tabs
        defaultValue="overview"
        className="w-full bg-[#FEFEFA] shadow justify-items-start"
      >
        <TabsList className="flex bg-[#FEFEFA]">
          {allTabData.map(({ value, label }) => (
            <TabsTrigger
              key={value}
              value={value}
              className={`
              relative px-4 py-2 text-lg font-medium text-gray-600
              bg-[#FEFEFA] hover:bg-[#D0D0B7]
              data-[state=active]:bg-gray-100
              data-[state=active]:text-gray-900
              data-[state=active]:after:content-['']
              data-[state=active]:after:absolute
              data-[state=active]:after:bottom-0
              data-[state=active]:after:left-0
              data-[state=active]:after:w-full
              data-[state=active]:after:h-0.5
              data-[state=active]:after:bg-[#F1502F]
              rounded-none border-none shadow-none focus:outline-none
              transition-all
            `}
            >
              {label}
            </TabsTrigger>
          ))}
        </TabsList>

        <TabsContent value="overview">
          {/* OVERVIEW */}
          <OverviewPage />
        </TabsContent>
        <TabsContent value="metrics">
          {/* METRICS */}
          <MetricsPage />
        </TabsContent>
        <TabsContent value="scaling">{/* SCALING */}</TabsContent>
      </Tabs>
    </div>
  );
}
