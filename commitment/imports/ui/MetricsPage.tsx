import React from "react";
import { useLocation } from "react-router-dom";
import MetricsTabs from "./components/metrics-page/MetricsTab";
import TopBar from "./components/metrics-page/TopBar";

const MetricsPage: React.FC = () => {
  const location = useLocation();
  const tab = location.state?.tab || "metrics";
  
  return (
    <div className="m-0 scroll-smooth flex flex-col">
      <TopBar />
      <MetricsTabs defaultTab={tab} />
    </div>
  );
};

export default MetricsPage;
