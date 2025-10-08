import React, { useState } from "react";
import { Subject } from "rxjs";
import MetricsTabs from "../components/metrics/MetricsTab";
import TopBar from "../components/metrics/TopBar";
import { useToast } from "../hooks/useToast";

const MetricsPage: React.FC = () => {
  const [refreshTrigger, setRefreshTrigger] = useState(0);

  const handleRefresh = () => {
    setRefreshTrigger((prev: number) => prev + 1);
  };

  return (
    <div className="m-0 scroll-smooth flex flex-col">
      <TopBar onRefresh={handleRefresh} />
      <MetricsTabs refreshTrigger={refreshTrigger} />
    </div>
  );
};

export default MetricsPage;
