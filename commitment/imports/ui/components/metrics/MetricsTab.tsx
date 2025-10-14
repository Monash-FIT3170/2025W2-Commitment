import {
  Tabs,
  TabsList,
  TabsTrigger,
  TabsContent,
} from "@base/tabs";
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
  const [activeTab, setActiveTab] = React.useState("metrics"); // ✅ Add this

  const visibleTabs = allTabData.filter(
    (tab) => !tab.requiresAuth || isLoggedIn
  );

  return (
    <Tabs
      value={activeTab} // ✅ Use controlled tab value
      onValueChange={setActiveTab} // ✅ Allow switching
      className="w-full bg-git-bg-elevated justify-items-start"
    >
      <TabsList className="w-full flex justify-start bg-git-bg-elevated">
        {visibleTabs.map(({ value, label }) => (
          <TabsTrigger key={value} value={value}>
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
