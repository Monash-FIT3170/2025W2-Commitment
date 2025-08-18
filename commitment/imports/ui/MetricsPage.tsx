import React from "react";
import { Meteor } from "meteor/meteor";
import { useTracker } from "meteor/react-meteor-data";
import MetricsTabs from "./components/metrics-page/MetricsTab";
import TopBar from "./components/metrics-page/TopBar";
import NavBar from "./components/landing-page/NavBar";

const MetricsPage: React.FC = () => {
  const user = useTracker(() => Meteor.user());
  const isLoggedIn = !!user;

  return (
    <div className="m-0 scroll-smooth flex flex-col">
      <NavBar isLoggedIn={isLoggedIn} />
      <TopBar />
      <MetricsTabs />
    </div>
  );
};

export default MetricsPage;
