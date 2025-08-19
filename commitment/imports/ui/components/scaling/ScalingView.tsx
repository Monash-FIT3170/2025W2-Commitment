"use client";

import React, { useState, useEffect } from "react";
import ScalingConfigForm from "./ScalingConfigForm";
import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogHeader,
  DialogTitle,
} from "../ui/dialog";

import { Button } from "../ui/button";
import GradingSheetForm from "./GradingSheetForm";
import ScalingSummary from "./ScalingSummary";
import type { UserScalingSummary } from "@server/commitment_api/types";

interface ScalingConfig {
  metrics: string[];
  method: string;
  customScript?: File[];
}

function ScalingView() {
  const [completed, setCompleted] = useState(false);

  // Step of scaling config wizard
  const [hasLoaded, setHasLoaded] = useState(false);
  const [step, setStep] = useState<"config" | "sheet" | "done">("config");
  const [showDialog, setShowDialog] = useState(false);
  const [config, setConfig] = useState<ScalingConfig | null>(null);
  const [gradingSheet, setGradingSheet] = useState<File | null>(null);

  // Load from localStorage on first mount
  useEffect(() => {

    // Grab from local storage first
    const lsCompleted = localStorage.getItem("hasVisitedScaling") === "true";
    setCompleted(lsCompleted);
    if (!lsCompleted) setShowDialog(true);
    setHasLoaded(true);
  }, []);

  // Persist to localStorage
  useEffect(() => {
    if (hasLoaded) {
      localStorage.setItem("hasVisitedScaling", completed ? "true" : "false");
    }
  }, [completed, hasLoaded]);

  const handleConfigSubmit = (configData: ScalingConfig) => {
    setConfig(configData);
    console.log("Config submitted:", configData);
    setStep("sheet");
  };

  const handleSheetSubmit = (sheetFile: File) => {
    setGradingSheet(sheetFile || null);
    setCompleted(true);
    setShowDialog(false);
    setStep("done");
  };

  // This is the variable that must store the final grades, scalings, aliases and name of contributors
  const userScalingSummaries: UserScalingSummary[] = [];

return (
  <div className="m-0 scroll-smooth">
    <div className="flex flex-col gap-32">
      <div className="max-w-[1600px] mx-20 rounded-2xl bg-white p-8">
        {/* Show "Create New Scaling" button if no completed config */}
        {!(completed && config) && (
          <Button
            className="bg-git-int-primary text-git-int-text hover:bg-git-int-primary-hover"
            onClick={() => {
              setStep("config");
              setShowDialog(true);
            }}
          >
            Create New Scaling
          </Button>
        )}

        {/* Show summary and options if completed */}
        {completed && step === "done" && config && (
          <div>
            {completed && config && (
              <ScalingSummary
                userScalingSummaries={userScalingSummaries}
                hasGradingSheet={!!gradingSheet}
              />
            )}

            <div className="flex justify-center gap-6 p-4">
              <Button
                className="bg-git-int-primary text-git-int-text hover:bg-git-int-primary-hover"
                onClick={() => {
                  setStep("sheet");
                  setShowDialog(true);
                }}
              >
                Upload Grading Sheet
              </Button>
              <Button
                className="bg-git-int-primary text-git-int-text hover:bg-git-int-primary-hover"
                onClick={() => {
                  setStep("config");
                  setShowDialog(true);
                }}
              >
                Generate New Scaling
              </Button>
            </div>
          </div>
        )}

        {/* Multi-Step Dialog */}
        <Dialog
          open={showDialog}
          onOpenChange={(open) => {
            if (!open && step === "sheet") {
              // if user closed grading sheet with cross button
              setCompleted(true);
              setStep("done");
            }
            setShowDialog(open);
          }}
        >
          <DialogContent className="max-w-full">
              {step === "config" && (
                <ScalingConfigForm onSubmit={handleConfigSubmit} />
              )}
              {step === "sheet" && (
                <GradingSheetForm onSubmit={handleSheetSubmit} />
              )}
            </DialogContent>
          </Dialog>
        </div>
      </div>
    </div>
  );
}

export default ScalingView;
