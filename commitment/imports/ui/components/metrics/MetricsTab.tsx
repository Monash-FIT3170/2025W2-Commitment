import { Tabs, TabsList, TabsTrigger, TabsContent } from "@base/tabs";
import React from "react";
import { AnalyticsView } from "./AnalyticsView";
import ScalingView from "../scaling/ScalingView";
import { useAuth } from "@hook/useAuth";

interface TabData {
  value: string;
  label: string;
  requiresAuth?: boolean;
}

const allTabData: TabData[] = [
  {
    value: "metrics",
    label: "Metrics",
  },
  {
    value: "scaling",
    label: "Scaling",
    requiresAuth: true,
  },
];

export default function MetricsTabs() {
  const isLoggedIn = useAuth();
  const [activeTab, setActiveTab] = React.useState("metrics");

  const visibleTabs = allTabData.filter(
    (tab) => !tab.requiresAuth || isLoggedIn
  );

  return (
    <Tabs
      value={activeTab}
      onValueChange={setActiveTab}
      className="w-full bg-git-bg-elevated dark:bg-git-bg-primary justify-items-start"
    >
      <TabsList className="w-full flex justify-start bg-git-bg-elevated">
        {visibleTabs.map(({ value, label }) => (
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
              data-[state=active]:after:bg-orange-500
              rounded-none border-none shadow-none focus:outline-hidden
              transition-all
            `}
          >
            {label}
          </TabsTrigger>
        ))}
      </TabsList>

      <TabsContent value="metrics">
        <AnalyticsView />
      </TabsContent>

      {isLoggedIn && (
        <TabsContent value="scaling" className="w-full">
          {/* ✅ Pass down a callback to go back to metrics */}
          <ScalingView onNavigateToMetrics={() => setActiveTab("metrics")} />
        </TabsContent>
      )}
    </Tabs>
  );
}
