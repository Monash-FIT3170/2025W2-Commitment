import {
  Tabs,
  TabsList,
  TabsTrigger,
  TabsContent,
} from "@ui/components/ui/tabs";
import React from "react";
import { OverviewPage } from "./OverviewPage";
import { AnalyticsView } from "./AnalyticsView";
import ScalingView from "../scaling/ScalingView";

interface TabData {
  value: string;
  label: string;
}

const allTabData: TabData[] = [
  {
    value: "metrics",
    label: "Metrics",
  },
  {
    value: "scaling",
    label: "Scaling",
  },
];

export default function MetricsTabs() {
  return (
    <Tabs
      defaultValue="metrics"
      className="w-full bg-git-bg-primary shadow-sm justify-items-start "
    >
      <TabsList className="w-full flex justify-start bg-git-bg-elevated ">
        {allTabData.map(({ value, label }) => (
          <TabsTrigger
            key={value}
            value={value}
            className={`
              relative px-4 text-lg font-medium text-foreground
              bg-git-bg-elevated hover:bg-git-tabs-hovered

              border-b border-git-stroke-primary/40
              data-[state=active]:bg-git-tabs-active
              data-[state=active]:git-tabs-hovered
              data-[state=active]:after:content-['']
              data-[state=active]:after:absolute
              data-[state=active]:after:bottom-0
              data-[state=active]:after:left-0
              data-[state=active]:after:w-full
              data-[state=active]:after:h-0.5
              data-[state=active]:after:bg-git
              data-[state=active]:after:bg-orange-500
              
              rounded-none border-none shadow-none focus:outline-hidden
              transition-all
            `}
          >
            {label}
          </TabsTrigger>
        ))}
      </TabsList>

      <TabsContent value="metrics" className="">
        {/* METRICS */}
        <div className="">
          <AnalyticsView />
        </div>
      </TabsContent>
      <TabsContent
        value="scaling"
        className=" w-full"
      >
        {/* SCALING */}
        <ScalingView />
      </TabsContent>
    </Tabs>
  );
}
