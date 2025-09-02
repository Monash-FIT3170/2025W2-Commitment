"use client";

import React, { useState, useEffect } from "react";
import ScalingConfigForm from "./ScalingConfigForm";
import {
  Dialog,
  DialogContent,
} from "../ui/dialog";

import { Button } from "../ui/button";
import GradingSheetForm from "./GradingSheetForm";
import ScalingSummary from "./ScalingSummary";
import type { UserScalingSummary } from "../../../api/types";
import type { GradingSheetRow } from "../utils/GradingSheetParser";

interface ScalingConfig {
  metrics: string[];
  method: string;
  customScript?: File[];
}

function ScalingView(): JSX.Element {
  const [completed, setCompleted] = useState(false);

  // Step of scaling config wizard
  const [hasLoaded, setHasLoaded] = useState(false);
  const [step, setStep] = useState<"config" | "sheet" | "done">("config");
  const [showDialog, setShowDialog] = useState(false);
  const [config, setConfig] = useState<ScalingConfig | null>(null);
  const [gradingSheet, setGradingSheet] = useState<File | null>(null);
  const [scaledResults, setScaledResults] = useState<UserScalingSummary[]>([]);

  // Function to calculate final grades by matching contributors with grading sheet data
  const calculateFinalGrades = (scalingResults: UserScalingSummary[], gradingData: GradingSheetRow[]): UserScalingSummary[] => 
    scalingResults.map(contributor => {
      // Try to find matching student in grading sheet by name or email
      const matchingStudent = gradingData.find(student => {
        const studentName = student.fullName.toLowerCase().trim();
        const contributorName = contributor.name.toLowerCase().trim();
        const studentEmail = student.emailAddress.toLowerCase().trim();
        
        if (studentName === contributorName) {
          return true;
        }
        
        return contributor.aliases.some(alias => 
          alias.email.toLowerCase().trim() === studentEmail
        );
      });

      if (matchingStudent) {
        const percentageGrade = (matchingStudent.grade / matchingStudent.maximumGrade) * 100;
        const finalGrade = percentageGrade * contributor.scale;        
        return {
          ...contributor,
          finalGrade: Math.round(finalGrade * 100) / 100
        };
      }
      
      // No matching student found
      return {
        ...contributor,
        finalGrade: null
      };
    });

  // Load from localStorage on first mount
  useEffect(() => {
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

  const handleConfigSubmit = (
    configData: ScalingConfig,
    results: UserScalingSummary[]
  ) => {
    setConfig(configData);
    setScaledResults(results);
    setStep("sheet");
  };

  const handleSheetSubmit = (sheetFile: File, parsedData?: GradingSheetRow[]) => {
    setGradingSheet(sheetFile);
    
    // Calculate final grades when grading sheet is provided
    if (parsedData && scaledResults.length > 0) {
      const updatedResults = calculateFinalGrades(scaledResults, parsedData);
      setScaledResults(updatedResults);
    }

    setCompleted(true);
    setShowDialog(false);
    setStep("done");
  };

  const handleSkipSheet = () => {
    setGradingSheet(null);
    setCompleted(true);
    setShowDialog(false);
    setStep("done");
  };

  return (
    <div className="m-0 scroll-smooth">
      <div className="flex flex-col gap-32">
        <div className="max-w-[1600px] mx-20 rounded-2xl bg-white p-8">
          {/* Always render the scaling summary in the background */}
          {config && scaledResults.length > 0 && (
            <div className="mb-6">
              <ScalingSummary
                userScalingSummaries={scaledResults}
                hasGradingSheet={!!gradingSheet}
              />
            </div>
          )}

          {/* Buttons for grading sheet or regenerate */}
          <div className="flex justify-center gap-6 p-4">
            <Button
              className="bg-git-int-primary text-git-int-text hover:bg-git-int-primary-hover"
              onClick={() => {
                setGradingSheet(null); // Reset grading sheet when creating new scaling
                setStep("config");
                setShowDialog(true);
              }}
            >
              Create New Scaling
            </Button>

            <Button
              className="bg-git-int-primary text-git-int-text hover:bg-git-int-primary-hover"
              onClick={() => {
                setStep("sheet");
                setShowDialog(true);
              }}
            >
              Upload Grading Sheet
            </Button>
          </div>

          {/* Multi-Step Dialog */}
          <Dialog
            open={showDialog}
            onOpenChange={(open) => {
              if (!open && step === "sheet") {
                setCompleted(true);
                setStep("done");
              }
              setShowDialog(open);
            }}
          >
            <DialogContent className="max-w-2xl">
              {step === "config" && (
                <ScalingConfigForm onSubmit={handleConfigSubmit} />
              )}
              {step === "sheet" && (
                <GradingSheetForm 
                  onSubmit={handleSheetSubmit} 
                  onSkip={handleSkipSheet}
                />
              )}
            </DialogContent>
          </Dialog>
        </div>
      </div>
    </div>
  );
}

export default ScalingView;
