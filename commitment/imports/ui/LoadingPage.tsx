import React, { useEffect, useState } from "react";
import { Meteor } from "meteor/meteor";
import { useLocation, useNavigate, Navigate } from "react-router-dom";
import { Subject } from "rxjs";

import LoadingBar from "@ui/components/Loading-page/LoadingBar";
import TipBox from "@ui/components/Loading-page/TipBox";
import NavBar from "@ui/components/landing-page/NavBar";
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
      setTipIndex(i => (i + 1) % tips.length);
    }, 4000);
    return () => Meteor.clearInterval(id);
  }, []);

  // Fetch repository data and stream notifier
  useEffect(() => {
    if (!repoUrl) return;

    const notifier = new Subject<string>();
    const sub = notifier.subscribe(msg => setMessage(msg));

    // Track validation progress
    let validationComplete = false;
    let hasError = false;
    let errorHandled = false; // Prevent multiple error handling

    // Add timeout to prevent hanging
    const validationTimeout = setTimeout(() => {
      if (!validationComplete && !hasError) {
        hasError = true;
        notifier.next("Validation timeout - repository may not exist or be accessible");
        setTimeout(() => {
          navigate("/home", { replace: true });
        }, 5000);
      }
    }, 60000); // 60 second timeout

    fetchRepo(repoUrl, notifier)
      .then(() => {
        // Don't show success message immediately - wait for actual validation
        console.log('Server method called, waiting for validation...');
      })
      .catch(err => {
        if (!errorHandled) {
          errorHandled = true;
          hasError = true;
          notifier.next(`Error: ${err}`);
          // Wait 10 seconds to let user read the error, then redirect back
          setTimeout(() => {
            navigate("/home", { replace: true });
          }, 10000);
        }
      });

    // Listen for specific validation messages to determine when to proceed
    const messageSubscription = notifier.subscribe((msg) => {
      // Don't process messages if error is already handled
      if (errorHandled) return;
      
      console.log('Received message:', msg);
      
      if (msg.includes('Repository validation failed') && !errorHandled) {
        errorHandled = true;
        hasError = true;
        clearTimeout(validationTimeout); // Clear timeout since we're handling the error
        // Don't send this back through the notifier - just set the message directly
        setMessage(`Validation failed: Repository does not exist or is not accessible`);
        setTimeout(() => {
          navigate("/home", { replace: true });
        }, 5000);
      } else if (msg.includes('Data processed!') && !validationComplete && !hasError) {
        // Only show success when data is actually processed
        validationComplete = true;
        clearTimeout(validationTimeout); // Clear timeout since validation succeeded
        notifier.next("Repository data loaded!");
        setProgress(100);

        // Add verification that data is actually saved before redirecting
        setTimeout(async () => {
          try {
            // Verify data is in database before redirecting
            const result = await Meteor.call('repoCollection.getData', repoUrl);
            if (result) {
              console.log('Data verified in database, redirecting...');
              navigate("/metrics", { replace: true, state: { repoUrl } });
            }
          } catch (error) {
            console.error('Data verification failed:', error);
            // If verification fails, try again after a longer delay
            setTimeout(() => {
              navigate("/metrics", { replace: true, state: { repoUrl } });
            }, 3000);
          }
        }, 2000);
      }
    });

    return () => {
      sub.unsubscribe();
      messageSubscription.unsubscribe();
      notifier.complete();
      clearTimeout(validationTimeout); // Clear timeout on component unmount
    };
  }, [repoUrl, navigate]);

  // If no repo URL provided, redirect immediately
  if (!repoUrl) return <Navigate to="/insert-git-repo" replace />;

  const containerClasses = [
    darkMode ? "dark" : "",
    "min-h-screen bg-background text-foreground",
  ]
    .filter(Boolean)
    .join(" ");

  return (
    <div className={containerClasses}>
      <div className="fixed top-0 w-full z-10">
        <NavBar isLoggedIn={!!Meteor.userId && !!Meteor.userId()} />
      </div>
      <div className="flex flex-col items-center justify-center h-screen pt-24 px-6">
        {/* Live notifier message */}
        <h2 className="text-3xl font-inconsolata-bold mb-6">
          {message}
        </h2>

        <LoadingBar progress={progress} />

        {/* Rotating tips */}
        <TipBox tip={tips[tipIndex]} />
      </div>
    </div>
  );
};

export default LoadingPage;
