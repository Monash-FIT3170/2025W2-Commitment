import React, { useState } from "react";
import { Subject } from "rxjs";
import MetricsTabs from "../components/metrics/MetricsTab";
import TopBar from "../components/metrics/TopBar";
import { useToast } from "../hooks/useToast";

const MetricsPage: React.FC = () => {
  return (
    <div className="m-0 scroll-smooth flex flex-col">
      <TopBar />
      <MetricsTabs />
    </div>
  );
};

export default MetricsPage;
