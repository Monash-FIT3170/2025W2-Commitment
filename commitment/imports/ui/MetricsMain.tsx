import React from "react";
import { MetricsTabs } from "./components/metrics-page/MetricsTab";
import { TopBar } from "./components/metrics-page/TopBar";

export const MetricsMain = () => (
  <div className="m-0 scroll-smooth flex flex-col">
    <TopBar />
    <MetricsTabs />
  </div>
);
