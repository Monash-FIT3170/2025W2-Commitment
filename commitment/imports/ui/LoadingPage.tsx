import React, { useEffect, useState } from "react";
import { Meteor } from "meteor/meteor";
import { useLocation, useNavigate, Navigate } from "react-router-dom";
import { Subject } from "rxjs";

import LoadingBar from "/imports/ui/components/loading/LoadingBar";
import TipBox from "/imports/ui/components/loading/TipBox";
import { fetchRepo } from "../api/call_repo";

interface LocationState {
  repoUrl?: string;
}
const LoadingPage: React.FC<{ darkMode?: boolean }> = ({ darkMode = false }) => {
  const navigate = useNavigate();
  const { state } = useLocation();
  const { repoUrl } = (state as LocationState) || {};

  // Notifier message replaces header
  const [message, setMessage] = useState<string>("Starting…");

  // Rotating tips
  const [tipIndex, setTipIndex] = useState<number>(0);
  const tips = [
    "Tip 1: Use keyboard shortcuts to improve productivity.",
    "Tip 2: Frequently commit your code to avoid loss.",
    "Tip 3: Write meaningful commit messages.",
  ];

  // Progress bar state
  const [progress, setProgress] = useState<number>(0);

  // Rotate tips every 4s
  useEffect(() => {
    const id = Meteor.setInterval(() => {
      setTipIndex((i) => (i + 1) % tips.length);
    }, 4000);
    return () => Meteor.clearInterval(id);
  }, []);

  // Fetch repository data and stream notifier
  useEffect(() => {
    if (!repoUrl) return;

    const notifier = new Subject<string>();
    const sub = notifier.subscribe(setMessage);
    // const $sout = notifier.subscribe(console.log) // for debugging purposes

    fetchRepo(repoUrl, notifier)
      .then((_: boolean) => {
        notifier.next("Repository data loaded!");
        setProgress(100);

        // redirect to the metrics page
        setTimeout(() => {
          navigate("/metrics", { replace: true, state: { repoUrl } });
        }, 1000);
      })
      .catch((err) => {
        notifier.next(`${err}`);
        // Wait 10 seconds to let user read the error, then redirect back
        setTimeout(() => {
          navigate("/home", { replace: true });
        }, 20000);
      });

    return () => {
      sub.unsubscribe();
      // $sout.unsubscribe()
      notifier.complete();
    };
  }, [repoUrl, navigate]);

  // If no repo URL provided, redirect immediately
  if (!repoUrl) return <Navigate to="/insert-git-repo" replace />;

  return (
    <>
      <div className="fixed top-0 w-full z-10" />
      <div className="flex flex-col items-center justify-center h-screen pt-24 px-6">
        {/* Live notifier message */}
        <h2 className="text-3xl font-inconsolata-bold mb-6">{message}</h2>

        <LoadingBar progress={progress} />

        {/* Rotating tips */}
        <TipBox tip={tips[tipIndex]} />
      </div>
    </>
  );
};

export default LoadingPage;
