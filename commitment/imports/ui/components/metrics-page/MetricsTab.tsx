import { Tabs, TabsList, TabsTrigger, TabsContent } from "@ui/components/ui/tabs";
import React from "react";
import { OverviewPage } from "../../OverviewPage";
import { MetricsPage } from "../../MetricsPage";

export function MetricsTabs() {
  return (
    <Tabs defaultValue="overview" className="w-full bg-white shadow">
      <TabsList className="flex bg-white px-2">
        {[
          { value: "overview", label: "Overview" },
          { value: "metrics", label: "Metrics" },
          { value: "scaling", label: "Scaling" },
          { value: "contributions", label: "Contributions" },
        ].map(({ value, label }) => (
          <TabsTrigger
            key={value}
            value={value}
            className={`
              relative px-4 py-2 text-sm font-medium text-gray-600
              data-[state=active]:bg-gray-100
              data-[state=active]:text-gray-900
              data-[state=active]:after:content-['']
              data-[state=active]:after:absolute
              data-[state=active]:after:bottom-0
              data-[state=active]:after:left-0
              data-[state=active]:after:w-full
              data-[state=active]:after:h-0.5
              data-[state=active]:after:bg-[#F1502F]
              bg-white rounded-none border-none shadow-none focus:outline-none
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
      <TabsContent value="scaling">
        {/* SCALING */}
      </TabsContent>
      <TabsContent value="contributions">
        {/* CONTRIBUTIONS */}
      </TabsContent>
    </Tabs>
  );
}
