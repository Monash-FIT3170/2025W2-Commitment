import React, { useState } from "react";
import MetricsTabs from "../components/metrics/MetricsTab";
import TopBar from "../components/metrics/TopBar";

const MetricsPage: React.FC = () => {
  const [refreshTrigger, setRefreshTrigger] = useState(0);

  const handleRefresh = () => {
    setRefreshTrigger((prev) => prev + 1);
  };

  return (
    <div className="m-0 scroll-smooth flex flex-col">
      <TopBar onRefresh={handleRefresh} />
      <MetricsTabs refreshTrigger={refreshTrigger} />
    </div>
  );
};

export default MetricsPage;
