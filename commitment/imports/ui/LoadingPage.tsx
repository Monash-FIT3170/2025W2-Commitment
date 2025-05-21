import React, { useEffect, useState } from "react";
import { Meteor } from "meteor/meteor";
import LoadingBar from "@ui/components/Loading-page/LoadingBar";
import TipBox from "@ui/components/Loading-page/TipBox";
import NavBar from "@ui/components/landing-page/NavBar";

declare global {
  interface Window {
    setLoadingBarProgress?: (value: number) => void;
  }
}

interface LoadingPageProps {
  darkMode?: boolean;
}

const LoadingPage: React.FC<LoadingPageProps> = ({ darkMode = false }) => {
  const [progress, setProgress] = useState(0);
  const [tipIndex, setTipIndex] = useState(0);
  const tips = [
    "Tip 1: Use keyboard shortcuts to improve productivity.",
    "Tip 2: Frequently commit your code to avoid loss.",
    "Tip 3: Write meaningful commit messages.",
  ];

  // rotate tips every 4s
  useEffect(() => {
    const id = Meteor.setInterval(() => {
      setTipIndex(i => (i + 1) % tips.length);
    }, 4000);
    return () => Meteor.clearInterval(id);
  }, []);

  // expose quick setter on window for testing
  useEffect(() => {
    window.setLoadingBarProgress = (val: number) => {
      setProgress(Math.max(0, Math.min(100, val)));
    };
    return () => {
      delete window.setLoadingBarProgress;
    };
  }, []);


  const containerClasses = [
    darkMode ? "dark" : "",
    "min-h-screen",
    "bg-background",
    "text-foreground",
  ]
    .filter(Boolean)
    .join(" ");

  return (
    <div className={containerClasses}>
      <div className="w-full fixed top-0 left-0 z-10">
        <NavBar />
      </div>
      <div className="flex flex-col items-center justify-center h-screen pt-24 px-6">
        <h2 className="text-3xl font-inconsolata-bold mb-6">
          Loading your data...
        </h2>

        <LoadingBar progress={progress} />

        <TipBox tip={tips[tipIndex]} />
      </div>
    </div>
  );
};

export default LoadingPage;
