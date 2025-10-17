import React from "react";
import MetricsTabs from "../components/metrics/MetricsTab";
import TopBar from "../components/metrics/TopBar";

const MetricsPage: React.FC = () => {
  return (
    <div className="m-0 scroll-smooth flex flex-col">
      <TopBar />
      <MetricsTabs />
    </div>
  );
};

export default MetricsPage;
