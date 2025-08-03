import React, { useState, useEffect } from "react";
import { ScalingConfigForm } from "./ScalingConfigForm";
import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogHeader,
  DialogTitle,
  DialogTrigger,
} from "../ui/dialog";

import { Button } from "../ui/button";
import { GradingSheetForm } from "./GradingSheetForm";

interface ScalingConfig {
  metrics: string[];
  method: string;
  customScript?: File;
}

export const ScalingView = () => {
  // Grab from local storage, defines if completed or not
  const [completed, setCompleted] = useState(false);

  // Step of scaling config wizard
  const [step, setStep] = useState<"config" | "sheet" | "done">("config");
  const [showDialog, setShowDialog] = useState(false);

  // Shared state for config and grading sheet
  const [config, setConfig] = useState<ScalingConfig | null>(null);
  const [gradingSheet, setGradingSheet] = useState<File | null>(null);

  // Flow for first visits
  useEffect(() => {
    // Grab from local storage first
    const lsCompleted = localStorage.getItem("hasVisitedScaling") === "true";
    setCompleted(lsCompleted);
    setShowDialog(!completed); //Opens automatically if we haven't made scaling yet
  }, []);

  const handleConfigSubmit = (configData: ScalingConfig) => {
    setConfig(configData);
    setStep("sheet");
  };

  const handleSheetSubmit = (sheetFile: File) => {
    setGradingSheet(sheetFile);
    setStep("done");
    setCompleted(true);
    setShowDialog(false);
  };

  return (
    <div className="m-0 scroll-smooth">
      <div className="flex flex-col gap-32">
        <div className="max-w-[1600px] mx-20 rounded-2xl bg-white p-8">
          {/* DEFAULT BACKGROUND */}
          <Button
            onClick={() => {
              setStep("config");
              setShowDialog(true);
            }}
          >
            Create New Scaling
          </Button>

          {/* MULTI STEP DIALOG */}
          <Dialog open={showDialog} onOpenChange={setShowDialog}>
            <DialogContent className="max-w-full">
              {step === "config" && (
                <ScalingConfigForm onSubmit={handleConfigSubmit} />
              )}
              {step === "sheet" && <GradingSheetForm />}
            </DialogContent>
          </Dialog>
        </div>
      </div>
    </div>
  );
};
