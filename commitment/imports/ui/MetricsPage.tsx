import React from "react";
import MetricsTabs from "./components/metrics-page/MetricsTab";
import TopBar from "./components/metrics-page/TopBar";

const MetricsPage: React.FC = () => {

  return (
    <div className="m-0 scroll-smooth flex flex-col">
      <TopBar />
      <MetricsTabs />
    </div>
  );
};

export default MetricsPage;
