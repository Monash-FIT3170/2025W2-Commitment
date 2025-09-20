"use client";

import React, { useState, useEffect } from "react";
import { Upload, Download, X } from "lucide-react";
import { useLocation } from "react-router-dom";
import ScalingConfigForm from "./ScalingConfigForm";
import { Dialog, DialogContent } from "@base/dialog";
import {
  AlertDialog,
  AlertDialogTrigger,
  AlertDialogContent,
  AlertDialogHeader,
  AlertDialogTitle,
  AlertDialogDescription,
  AlertDialogFooter,
  AlertDialogCancel,
  AlertDialogAction,
} from "@base/alert-dialog";
import {
  calculateFinalGrades,
  generateScaledGradingSheet,
} from "./ScalingUtils";

import { Button } from "@base/button";
import GradingSheetForm from "./GradingSheetForm";
import ScalingSummary from "./ScalingSummary";
import type {
  UnmappedContributor,
  UserScalingSummary,
} from "../../../api/types";
import type { GradingSheetRow, ParseResult } from "../utils/GradingSheetParser";
import { toast } from "@hook/use-toast";
import InfoButton from "@base/infoButton";
import { useNavigate } from "react-router-dom";
import { useAuth } from "@hook/useAuth";

interface ScalingConfig {
  metrics: string[];
  method: string;
  customScript?: File[];
}

function ScalingView(): JSX.Element {
  const location = useLocation();
  const [completed, setCompleted] = useState(false);

  const [hasLoaded, setHasLoaded] = useState(false);
  const [step, setStep] = useState<"config" | "sheet" | "done">("config");
  const [showDialog, setShowDialog] = useState(false);
  const [showClearDialog, setShowClearDialog] = useState(false);
  const [config, setConfig] = useState<ScalingConfig | null>(null);
  const [gradingSheet, setGradingSheet] = useState<File | null>(null);
  const [gradingSheetParseResult, setGradingSheetParseResult] =
    useState<ParseResult | null>(null);
  const [scaledResults, setScaledResults] = useState<UserScalingSummary[]>([]);
  const [repoUrl, setRepoUrl] = useState<string | null>(null);

  // for Alias Mapping
  const [unmappedUsers, setUnmappedUsers] = useState<
    { name: string; rawIdentifiers: string[] }[]
  >([]);
  const [showAliasDialog, setShowAliasDialog] = useState(false);

  const navigate = useNavigate();

  const isLoggedIn = useAuth();

  useEffect(() => {
    if (isLoggedIn === false) {
      navigate("/dashboard");
    }
  }, [isLoggedIn, navigate]);

  useEffect(() => {
    if (!repoUrl) return;

    Meteor.call(
      "aliasConfigs.validateAllContributors",
      repoUrl,
      (err: Meteor.Error | null, res: { unmapped: UnmappedContributor[] }) => {
        if (err) {
          console.error("Error validating contributors:", err);
          return;
        }

        if (res.unmapped.length > 0) {
          setUnmappedUsers(res.unmapped);
          setShowAliasDialog(true);
        } else {
          setShowAliasDialog(false);
        }
      }
    );
  }, [repoUrl]);

  useEffect(() => {
    if (!config || !gradingSheetParseResult) return;

    // Only recalc if there are previously scaled results
    if (scaledResults.length === 0) return;

    try {
      const parsedData = gradingSheetParseResult.data;
      if (parsedData && parsedData.length > 0) {
        const updatedResults = calculateFinalGrades(scaledResults, parsedData);
        setScaledResults(updatedResults);
      }
    } catch (err) {
      console.error(
        "Error recalculating scaled results after alias update",
        err
      );
    }
  }, [config, gradingSheetParseResult, unmappedUsers]);

  // Function to clear all scaling data from localStorage and reset state
  const clearScalingData: () => void = () => {
    localStorage.removeItem("hasVisitedScaling");
    localStorage.removeItem("scaling_config");
    localStorage.removeItem("scaling_results");
    localStorage.removeItem("scaling_grading_sheet_name");
    localStorage.removeItem("scaling_parse_result");
    localStorage.removeItem("scaling_step");

    // Reset all state
    setConfig(null);
    setGradingSheet(null);
    setGradingSheetParseResult(null);
    setScaledResults([]);
    setCompleted(false);
    setStep("config");
  };

  // Handle confirmed clear action
  const handleConfirmClear = () => {
    clearScalingData();
    setShowClearDialog(false);
    toast({
      title: "Scaling cleared",
      description: "All scaling configuration and data has been cleared.",
    });
  };

  // Simple initialization with localStorage persistence for core state
  useEffect(() => {
    const currentRepoUrl: string = location.state?.repoUrl ?? null;
    setRepoUrl(currentRepoUrl);

    // Check if repo has changed - clear localStorage if it has
    const lastRepoUrl = localStorage.getItem("scaling_last_repo_url");
    const hasExistingScalingData =
      localStorage.getItem("scaling_config") ||
      localStorage.getItem("scaling_results") ||
      localStorage.getItem("scaling_grading_sheet_name");

    if (
      (lastRepoUrl && lastRepoUrl !== currentRepoUrl) ||
      (hasExistingScalingData && !lastRepoUrl && currentRepoUrl)
    ) {
      clearScalingData();
      localStorage.setItem("scaling_last_repo_url", currentRepoUrl);
      setCompleted(false);
      setShowDialog(true);
    } else {
      if (currentRepoUrl) {
        // Store current repo URL as last_visited
        localStorage.setItem("scaling_last_repo_url", currentRepoUrl);
      }

      const hasVisited = localStorage.getItem("hasVisitedScaling") === "true";
      setCompleted(hasVisited);

      // Only show dialog on true first visit (no existing scaling data and not visited)
      if (!hasVisited && !hasExistingScalingData) {
        setShowDialog(true);
      }

      // Restore key state from localStorage only if repo hasn't changed
      try {
        const savedConfig = localStorage.getItem("scaling_config");
        const savedResults = localStorage.getItem("scaling_results");
        const savedGradingSheetName = localStorage.getItem(
          "scaling_grading_sheet_name"
        );
        const savedParseResult = localStorage.getItem("scaling_parse_result");
        const savedStep = localStorage.getItem("scaling_step");

        if (savedConfig) {
          const parsedConfig = JSON.parse(savedConfig) as ScalingConfig;
          setConfig(parsedConfig);
        }
        if (savedResults) {
          const parsedResults = JSON.parse(
            savedResults
          ) as UserScalingSummary[];
          setScaledResults(parsedResults);
        }
        if (savedGradingSheetName) {
          const placeholderFile = new File([""], savedGradingSheetName, {
            type: "text/csv",
          });
          setGradingSheet(placeholderFile);
        }
        if (savedParseResult) {
          const parsedParseResult = JSON.parse(savedParseResult) as ParseResult;
          setGradingSheetParseResult(parsedParseResult);
        }
        if (
          savedStep &&
          (savedStep === "config" ||
            savedStep === "sheet" ||
            savedStep === "done")
        ) {
          setStep(savedStep);
        }
      } catch {
        // Ignore errors in restoring state
      }
    }

    setHasLoaded(true);
  }, [location]);

  // Save all state changes to localStorage (combined auto-save)
  useEffect(() => {
    if (!hasLoaded) return;

    try {
      // Save scaling data
      if (config) {
        localStorage.setItem("scaling_config", JSON.stringify(config));
      }
      if (scaledResults.length > 0) {
        localStorage.setItem("scaling_results", JSON.stringify(scaledResults));
      }
      if (gradingSheet) {
        localStorage.setItem("scaling_grading_sheet_name", gradingSheet.name);
      }
      if (gradingSheetParseResult) {
        localStorage.setItem(
          "scaling_parse_result",
          JSON.stringify(gradingSheetParseResult)
        );
      }
      localStorage.setItem("scaling_step", step);
      if (completed) {
        localStorage.setItem("hasVisitedScaling", "true");
      }
    } catch {
      // Ignore localStorage errors
    }
  }, [
    config,
    scaledResults,
    gradingSheet,
    gradingSheetParseResult,
    step,
    completed,
    hasLoaded,
    repoUrl,
  ]);

  const handleConfigSubmit = (
    configData: ScalingConfig,
    results: UserScalingSummary[]
  ) => {
    setConfig(configData);
    if (gradingSheetParseResult) {
      const parsedData = gradingSheetParseResult.data;
      if (parsedData && parsedData.length > 0) {
        const updatedResults = calculateFinalGrades(results, parsedData);
        setScaledResults(updatedResults);
      } else {
        setScaledResults(results);
      }
    } else {
      setScaledResults(results);
    }
    setStep("sheet");
  };

  const handleSheetSubmit = (
    sheetFile: File,
    parsedData?: GradingSheetRow[],
    parseResult?: ParseResult
  ) => {
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
      const scaledFile = generateScaledGradingSheet(
        gradingSheetParseResult,
        scaledResults
      );

      // Create a filename with "scaled_" prefix
      const originalName = gradingSheet.name;
      const fileExtension = originalName.substring(
        originalName.lastIndexOf(".")
      );
      const nameWithoutExtension = originalName.substring(
        0,
        originalName.lastIndexOf(".")
      );
      const scaledFileName = `scaled_${nameWithoutExtension}${fileExtension}`;

      // Create a new file with the custom name
      const renamedFile = new File([scaledFile], scaledFileName, {
        type: scaledFile.type,
      });

      // Trigger download
      const url = URL.createObjectURL(renamedFile);
      const link = document.createElement("a");
      link.href = url;
      link.download = scaledFileName;
      document.body.appendChild(link);
      link.click();
      document.body.removeChild(link);
      URL.revokeObjectURL(url);
    } catch {
      toast({
        title: "Error",
        description: "Failed to download scaled grading sheet.",
        variant: "destructive",
      });
    }
  };

  return (
    <div className="w-full m-0 scroll-smooth p-10">
      <div className="flex flex-col gap-32">
        <div className="w-full px-4 sm:px-6 md:px-8 lg:px-12 xl:px-20 py-8 rounded-2xl bg-git-bg-elevated outline-2 outline-git-bg-secondary">
          {showAliasDialog && (
            <AlertDialog
              open={showAliasDialog}
              onOpenChange={setShowAliasDialog}
            >
              <AlertDialogTrigger asChild>
                <div></div>
              </AlertDialogTrigger>

              <AlertDialogContent>
                <AlertDialogHeader>
                  <AlertDialogTitle>Unmapped Contributors</AlertDialogTitle>
                  <AlertDialogDescription>
                    The following contributors are not mapped in your alias
                    config:
                    <ul className="mt-2 list-disc ml-5">
                      {unmappedUsers.map((u) => (
                        <li key={u.name}>
                          <strong>{u.name}</strong>
                        </li>
                      ))}
                    </ul>
                    Please upload or update your alias configuration in
                    settings.
                  </AlertDialogDescription>
                </AlertDialogHeader>
                <AlertDialogFooter className="p-0">
                  <div className="w-full flex justify-center items-center">
                    <AlertDialogAction
                      onClick={() => {
                        navigate("/settings", {
                          state: { tab: "alias-config" },
                        });
                      }}
                    >
                      Go to Alias Configuration
                    </AlertDialogAction>
                  </div>
                </AlertDialogFooter>
              </AlertDialogContent>
            </AlertDialog>
          )}

          {/* Always render the scaling summary in the background */}
          {config && scaledResults.length > 0 && !showAliasDialog && (
            <div className="mb-6">
              {/* Header */}
              <div className="mb-10">
                <div className="flex items-center gap-4">
                  <h1 className="text-5xl text-foreground font-robotoFlex">
                    Scaling
                  </h1>
                  <InfoButton description="Configure scaling and upload a grading sheet to evaluate contributors" />
                </div>
                <div className="h-[2px] bg-git-stroke-primary w-1/4 mt-2" />
              </div>
              <ScalingSummary
                userScalingSummaries={scaledResults}
                hasGradingSheet={!!gradingSheet}
              />
            </div>
          )}

          {/* Buttons for grading sheet or regenerate */}
          {!showAliasDialog && (
            <div className="flex justify-center gap-4 flex-wrap p-4">
              <Button
                className="bg-git-int-primary text-git-int-text hover:bg-git-int-primary-hover"
                onClick={() => {
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
                <Upload className="h-4 w-4" />
                {gradingSheet
                  ? "Replace Grading Sheet"
                  : "Upload Grading Sheet"}
              </Button>

              {gradingSheet && gradingSheetParseResult && (
                <Button
                  className="bg-git-int-primary text-git-int-text hover:bg-git-int-primary-hover"
                  onClick={() => {
                    void handleDownloadScaledSheet();
                  }}
                >
                  <Download className="h-4 w-4" />
                  Download Scaled Grading Sheet
                </Button>
              )}

              {(config || gradingSheet || scaledResults.length > 0) && (
                <AlertDialog
                  open={showClearDialog}
                  onOpenChange={setShowClearDialog}
                >
                  <AlertDialogTrigger asChild>
                    <Button className="bg-git-int-destructive text-white hover:bg-git-int-destructive-hover px-4 py-2">
                      Clear
                      <X className="h-4 w-4" />
                    </Button>
                  </AlertDialogTrigger>
                  <AlertDialogContent>
                    <AlertDialogHeader>
                      <AlertDialogTitle>Clear Scaling</AlertDialogTitle>
                      <AlertDialogDescription>
                        Are you sure you want to clear all scaling data? You
                        will need to reconfigure scaling settings and re-upload
                        your grading sheet if you do.
                      </AlertDialogDescription>
                    </AlertDialogHeader>
                    <AlertDialogFooter>
                      <AlertDialogCancel>Cancel</AlertDialogCancel>
                      <AlertDialogAction onClick={handleConfirmClear}>
                        Clear
                      </AlertDialogAction>
                    </AlertDialogFooter>
                  </AlertDialogContent>
                </AlertDialog>
              )}
            </div>
          )}

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
