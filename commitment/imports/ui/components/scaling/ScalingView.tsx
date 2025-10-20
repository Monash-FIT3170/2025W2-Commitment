"use client";

import React, { useState, useEffect } from "react";
import { Upload, Download, X } from "lucide-react";
import { useLocation } from "react-router-dom";
import ScalingConfigForm from "./ScalingConfigForm";
import { Dialog, DialogContent } from "@base/dialog";
import { Tabs, TabsContent, TabsList, TabsTrigger } from "@base/tabs";
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
import type { UnmappedContributor, UserScalingSummary } from "@api/types";
import type { GradingSheetRow, ParseResult } from "../utils/GradingSheetParser";
import { toast } from "@hook/useToast";
import InfoButton from "@base/infoButton";
import { useNavigate } from "react-router-dom";
import { useAuth } from "@hook/useAuth";
import CustomScriptExport from "./CustomScriptExport";
import { exportDataService } from "./CustomScriptExport/exportDataService";

interface ScalingConfig {
  metrics: string[];
  method: string;
  customScript?: File[];
}

interface ScalingViewProps {
  onNavigateToMetrics?: () => void;
}

function ScalingView({ onNavigateToMetrics }: ScalingViewProps): JSX.Element {
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
  const [availableBranches, setAvailableBranches] = useState<string[]>([]);

  // for Alias Mapping
  const [unmappedUsers, setUnmappedUsers] = useState<
    { name: string; rawIdentifiers: string[] }[]
  >([]);
  const [showAliasDialog, setShowAliasDialog] = useState(false);
  const [activeTab, setActiveTab] = useState<"scaling" | "export">("scaling");

  const navigate = useNavigate();

  const isLoggedIn = useAuth();

  useEffect(() => {
    if (!repoUrl || !isLoggedIn) return;

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
          setUnmappedUsers([]);
          setShowAliasDialog(false);
        }
      }
    );
  }, [repoUrl, isLoggedIn]);

  // Fetch available branches from server metadata when repoUrl is set
  useEffect(() => {
    if (!repoUrl) return;

    try {
      Meteor.call(
        "repo.getMetadata",
        repoUrl,
        (err: Meteor.Error | null, res: { branches: string[] } | undefined) => {
          if (err) {
            console.error("Error fetching repo metadata:", err);
            return;
          }
          if (res && Array.isArray(res.branches)) {
            setAvailableBranches(res.branches);
          }
        }
      );
    } catch (e) {
      console.error("Failed to load branches", e);
    }
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
    const currentRepoUrl: string | null =
      (location.state as { repoUrl?: string } | null)?.repoUrl ?? null;

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
      if (currentRepoUrl) {
        localStorage.setItem("scaling_last_repo_url", currentRepoUrl);
      }
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

    if (isLoggedIn) 
      setStep("sheet");
    else
      handleSkipSheet()
    
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

      const originalName = gradingSheet.name;
      const fileExtension = originalName.substring(
        originalName.lastIndexOf(".")
      );
      const nameWithoutExtension = originalName.substring(
        0,
        originalName.lastIndexOf(".")
      );
      const scaledFileName = `scaled_${nameWithoutExtension}${fileExtension}`;

      const renamedFile = new File([scaledFile], scaledFileName, {
        type: scaledFile.type,
      });

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

  // availableBranches is now fetched from repo metadata

  // Handle data request for export
  const handleDataRequest = async (config: any) => {
    if (!repoUrl) {
      throw new Error('No repository URL available');
    }
    return await exportDataService.fetchExportData(config, repoUrl);
  };

  return (
    <div className="w-full m-0 scroll-smooth border-t border-git-stroke-primary/40 bg-git-bg-elevated dark:bg-git-bg-primary">
      <div className="flex flex-col gap-32">
        <div className="w-full px-4 sm:px-6 md:px-8 lg:px-12 xl:px-20 py-8 rounded-2xl bg-git-bg-elevated outline-2 outline-git-bg-secondary">
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

          {/* Tabs */}
          <Tabs value={activeTab} onValueChange={(value) => setActiveTab(value as "scaling" | "export")} className="w-full">
            <TabsList className="grid w-full grid-cols-2 mb-8">
              <TabsTrigger value="scaling" className="text-base font-medium">Scaling Configuration</TabsTrigger>
              <TabsTrigger value="export" className="text-base font-medium">Custom Script Export</TabsTrigger>
            </TabsList>

            <TabsContent value="scaling" className="space-y-6">
              {showAliasDialog && (
            <AlertDialog
              open={showAliasDialog}
              onOpenChange={setShowAliasDialog}
            >
              <AlertDialogTrigger asChild />

              <AlertDialogContent>
                <div className="flex justify-between items-start">
                  <button
                    type="button"
                    onClick={() => {
                      setShowAliasDialog(false);
                      if (onNavigateToMetrics) {
                        onNavigateToMetrics();
                      }
                    }}
                    className="absolute top-2 right-2 p-1 rounded-md hover:bg-gray-100 transition-colors"
                    aria-label="Close"
                  >
                    <X className="w-5 h-5" />
                  </button>

                  {/* Optional: maintain centered title alignment visually */}
                  <AlertDialogHeader className="flex-1 text-center">
                    <AlertDialogTitle>Unmapped Contributors</AlertDialogTitle>
                  </AlertDialogHeader>
                </div>

                <AlertDialogDescription className="mt-3">
                  The following contributors are not mapped in your alias
                  config:
                  <ul className="mt-2 list-disc ml-5">
                    {unmappedUsers.map((u) => (
                      <li key={u.name}>
                        <strong>{u.name}</strong>
                      </li>
                    ))}
                  </ul>
                  Please upload or update your alias configuration in settings.
                </AlertDialogDescription>

                <AlertDialogFooter className="p-0 mt-4 flex justify-center">
                  <div className="flex justify-center w-full">
                    <AlertDialogAction
                      type="button"
                      className="inline-flex px-4 py-2 justify-center"
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

              {/* Display the grading sheet only to logged in users */}
              {isLoggedIn && (
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
              )}

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
            </TabsContent>

            <TabsContent value="export" className="space-y-6">
              <CustomScriptExport
                availableBranches={availableBranches}
                repoUrl={repoUrl || ''}
                onDataRequest={handleDataRequest}
              />
            </TabsContent>
          </Tabs>
        </div>
      </div>
    </div>
  );
}

export default ScalingView;
