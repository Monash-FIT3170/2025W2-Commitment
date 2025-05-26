import React from "react";
import { MetricsTabs } from "./components/metrics-page/MetricsTab";

export const TestPage = () => (
  <div className="m-0 scroll-smooth">
    <div className="flex flex-col gap-32">
      <MetricsTabs />
    </div>
  </div>
);
