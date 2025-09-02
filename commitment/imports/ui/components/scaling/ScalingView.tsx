"use client";

import React, { useState, useEffect } from "react";
import { Upload, Download } from "lucide-react";
import ScalingConfigForm from "./ScalingConfigForm";
import {
  Dialog,
  DialogContent,
} from "../ui/dialog";
import { calculateFinalGrades, generateScaledGradingSheet } from "./ScalingFunctions";

import { Button } from "../ui/button";
import GradingSheetForm from "./GradingSheetForm";
import ScalingSummary from "./ScalingSummary";
import type { UserScalingSummary } from "../../../api/types";
import type { GradingSheetRow, ParseResult } from "../utils/GradingSheetParser";

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
  const [gradingSheetParseResult, setGradingSheetParseResult] = useState<ParseResult | null>(null);
  const [scaledResults, setScaledResults] = useState<UserScalingSummary[]>([]);

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

  const handleSheetSubmit = (sheetFile: File, parsedData?: GradingSheetRow[], parseResult?: ParseResult) => {
    setGradingSheet(sheetFile);
    
    // Store the parse result for later use in generating scaled CSV
    if (parseResult) {
      setGradingSheetParseResult(parseResult);
    }
    
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
    if (!gradingSheet) {
      setGradingSheet(null);
      setGradingSheetParseResult(null);
    }
    setCompleted(true);
    setShowDialog(false);
    setStep("done");
  };

  const handleDownloadScaledSheet = async () => {
    if (!gradingSheetParseResult || !gradingSheet) return;
    
    try {
      // Generate the scaled grading sheet
      const scaledFile = await generateScaledGradingSheet(gradingSheetParseResult, scaledResults);
      
      // Create a filename with "scaled_" prefix
      const originalName = gradingSheet.name;
      const fileExtension = originalName.substring(originalName.lastIndexOf('.'));
      const nameWithoutExtension = originalName.substring(0, originalName.lastIndexOf('.'));
      const scaledFileName = `scaled_${nameWithoutExtension}${fileExtension}`;
      
      // Create a new file with the custom name
      const renamedFile = new File([scaledFile], scaledFileName, { type: scaledFile.type });
      
      // Trigger download
      const url = URL.createObjectURL(renamedFile);
      const link = document.createElement('a');
      link.href = url;
      link.download = scaledFileName;
      document.body.appendChild(link);
      link.click();
      document.body.removeChild(link);
      URL.revokeObjectURL(url);
    } catch (error) {
      console.error('Error generating scaled grading sheet:', error);
    }
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
                setGradingSheetParseResult(null); // Reset parse result
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
              <Upload className="h-4 w-4"/>
              {gradingSheet ? 'Replace Grading Sheet' : 'Upload Grading Sheet'}
            </Button>

            {/* Download button - only visible when grading sheet is uploaded */}
            {gradingSheet && gradingSheetParseResult && (
              <Button
                className="bg-git-int-primary text-git-int-text hover:bg-git-int-primary-hover"
                onClick={() => {
                  handleDownloadScaledSheet().catch(console.error);
                }}
              >
                <Download className="h-4 w-4"/>
                Download Scaled Grading Sheet
              </Button>
            )}
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
