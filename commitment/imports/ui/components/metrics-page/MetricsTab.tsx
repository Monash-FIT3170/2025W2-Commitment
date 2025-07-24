import {
  Tabs,
  TabsList,
  TabsTrigger,
  TabsContent,
} from "@ui/components/ui/tabs";
import React from "react";
import { OverviewPage } from "./OverviewPage";
import { MetricsPage } from "./MetricsPage";

interface TabData {
  value: string;
  label: string;
}

const allTabData: TabData[] = [
  // {
  //   value: "overview",
  //   label: "Overview",
  // },
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
    <Tabs
      defaultValue="metrics"
      className="w-full bg-[#FEFEFA] shadow-sm justify-items-start"
    >
      <TabsList className="flex bg-[#FEFEFA] border-b">
        {allTabData.map(({ value, label }) => (
          <TabsTrigger
            key={value}
            value={value}
            className={`
              relative px-4 text-lg font-medium text-gray-600
              bg-[#FEFEFA] hover:bg-[#D0D0B7]
              data-[state=active]:bg-gray-100
              data-[state=active]:text-gray-900
              data-[state=active]:after:content-['']
              data-[state=active]:after:absolute
              data-[state=active]:after:bottom-0
              data-[state=active]:after:left-0
              data-[state=active]:after:w-full
              data-[state=active]:after:h-0.5
              data-[state=active]:after:bg-git
              rounded-none border-none shadow-none focus:outline-hidden
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
  );
}
