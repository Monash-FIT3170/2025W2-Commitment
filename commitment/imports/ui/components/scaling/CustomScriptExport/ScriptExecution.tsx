import React, {useState} from 'react';
import {Download, FileCode2, FileText, LoaderCircle, Terminal, Upload} from 'lucide-react';
import {Card, CardContent, CardHeader, CardTitle} from '@base/card';
import {ExportHistoryItem} from './ExportHistory';
import ScriptSpecification, {
  initialScript
} from "@ui/components/scaling/CustomScriptExport/ScriptExecution/ScriptSpecification";
import {
  DataSelectionConfig,
  DataSelectionPanel,
  DataSelectionPanelProps
} from "@ui/components/scaling/CustomScriptExport/DataSelectionPanel";
import {useLocalStorage} from "@hook/useLocalStorage";
import {ExportData} from "@ui/components/scaling/CustomScriptExport/ExportPreview";
import useAsync from "@hook/useAsync";
import {PythonExecutorScalingResponse} from "@api/types";
import {meteorCallAsync} from "@api/meteor_interface";
import ScalingSummary from "@ui/components/scaling/ScalingSummary";
import {cn} from "@ui/lib/utils";
import ScriptEditor from "@ui/components/scaling/CustomScriptExport/ScriptExecution/ScriptEditor";
import type {GradingSheetRow, ParseResult} from "@ui/components/utils/GradingSheetParser";
import {Button} from "@base/button";
import {useAuth} from "@hook/useAuth";
import ScriptExecutionGradingSheetDialog
  from "@ui/components/scaling/CustomScriptExport/ScriptExecution/ScriptExecutionGradingSheetDialog";
import {generateScaledGradingSheet} from "@ui/components/scaling/ScalingUtils";
import {toast} from "@hook/useToast";

interface ScriptExecutionProps extends DataSelectionPanelProps {
  history: ExportHistoryItem[];
  onDataRequest: (config: DataSelectionConfig) => Promise<ExportData>;
  gradingSheet?: File | null;
  gradingSheetParseResult?: ParseResult | null,
  setStep?: (step: "config" | "sheet" | "done") => void,
  setShowDialog?: (value: boolean) => void,
  handleSheetSubmit?: (
    gradingSheet: File,
    parsedData?: GradingSheetRow[],
    parseResult?: ParseResult
  ) => void;
}

export const ScriptExecution: React.FC<ScriptExecutionProps> = ({
  history,
  onDataRequest,
  gradingSheet,
  gradingSheetParseResult,
  setStep,
  setShowDialog,
  handleSheetSubmit,
  ...dataSelectionPanelProps
}: ScriptExecutionProps) => {
  // const latestExport = history.length > 0 ? history[0] : null;

  const [code, setCode] = useLocalStorage('custom-execution-script', initialScript);
  const [currentConfig, setCurrentConfig] = useState<DataSelectionConfig | null>(null);
  const [response, setResponseRaw] = useState<PythonExecutorScalingResponse>({});

  const scaledResults = response.data ?? [];
  const [executionLoading, setExecutionLoading] = useState<boolean>(false);

  const isLoggedIn = useAuth();
  const [dialogOpen, setDialogOpen] = useState<boolean>(false);

  // Add side effect of toasting errors whenever the response is set
  const setResponse = (response: PythonExecutorScalingResponse) => {
    if (response.error !== undefined) {
      toast({
        title: "Execution Error",
        description: response.error,
        variant: "destructive",
      });
    }
    if ((response.stderr?.trim().length ?? 0) > 0) {
      const lines = response.stderr!.trimEnd().split('\n');
      const lastLine = lines[lines.length - 1];

      toast({
        title: "Script output to stderr",
        description: lastLine,
        variant: "destructive",
      });
    }

    setResponseRaw(response);
  }

  // const responseDataJson = useMemo(() => {
  //   if (response.data)
  //     return JSON.stringify(response.data, null, 2);
  //   return "";
  // }, [response.data]);

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

  const csv = useAsync<string | null, string>(async () => {
    if (currentConfig === null)
      return null;

    try {
      const data = await onDataRequest(currentConfig);

      const csvHeaders = data.headers.join(',');
      // Make sure any strings that include, are quoted
      const csvRows = data.rows
        .map(row => (
          row.map(cell => (
            typeof cell === 'number'
              ? cell
              : cell.includes(',') && !(cell.startsWith('"') && cell.endsWith('"')
                || cell.startsWith('\'') && cell.endsWith('\''))
                ? `"${cell}"`
                : cell
          ))
        ))
        .map(row => row.join(','))
        .join('\n');
      return `${csvHeaders}\n${csvRows}`;
    } catch (err) {
      throw (err instanceof Error ? err.message : 'Failed to load data');
    }
  }, [currentConfig])

  const onExecuteButton = () => {
    if (csv.data === null)
      return;

    // Call the API!
    setExecutionLoading(true);
    meteorCallAsync<PythonExecutorScalingResponse>("pythonExecutorScaling")(code, csv.data)
      .then(v => {
        setResponse(v);
        return v;
      })
      .catch((e) => {
        console.error("Caught error trying to use python executor: ", e);
        setResponse({
          error: "Failed to call python executor API"
        })
      })
      .finally(() => {
        setExecutionLoading(false);
      });
  }

  const errorOutput = response.error || response.stderr
    ? [response.stderr, response.error].filter(Boolean).join('\n')
    : undefined;

  return (
    <div className="space-y-6">
      {/* Main Script Execution Card */}
      <Card className="bg-git-bg-elevated dark:bg-git-bg-primary border-git-stroke-primary rounded-xl">
        <CardHeader className="bg-git-int-primary rounded-t-xl">
          <CardTitle className="text-git-int-text">Script Execution</CardTitle>
          <p className="text-sm text-git-int-text/90">
            Execute custom scripts using exported CSV data. This section is currently under development.
          </p>
        </CardHeader>
        <CardContent className="bg-git-bg-elevated dark:bg-git-bg-primary pt-6 rounded-b-xl">
          <div className="pb-3">
            <ScriptSpecification
              className={[
                "mb-6 mt-0 min-h-[40vh]",
                cn(
                  "mb-6 mt-0 min-h-[40vh] transition-opacity ease-out duration-200 ",
                  csv.loading && "opacity-50" || "opacity-100"
                )
              ]}
              code={[
                code,
                csv.data ?? ""
              ]}
              setCode={[
                setCode
              ]}
              name={[
                "script.py",
                "data.csv"
              ]}
              icon={[
                <FileCode2 size="sm"/>,
                csv.loading
                  ? <LoaderCircle size="sm" className="animate-spin"/>
                  : <FileText size="sm"/>
              ]}
              language={[
                "python",
                "csv"
              ]}
              readonly={[
                false,
                true
              ]}
            />

          </div>

          <div className="space-y-6">
            <DataSelectionPanel
              {...dataSelectionPanelProps}
              minimal={true}
              buttonLabel="Execute Script"
              onPreviewData={onExecuteButton}
              onConfigChange={(config) => {
                setCurrentConfig(config);
                dataSelectionPanelProps.onConfigChange?.(config);
              }}
            />

            <ScriptSpecification
              className={cn(
                "mb-6 transition-opacity ease-out duration-200 ",
                executionLoading && "opacity-50" || "opacity-100"
              )}
              code={response.stdout?.trimEnd() ?? "# Press 'Execute Script' to see output here"}
              setCode={() => {}}
              name={"Execution Output"}
              icon={
                executionLoading
                  ? <LoaderCircle size="sm" className="animate-spin"/>
                  : <Terminal size="sm"/>
              }
              language="python"
              readonly
            >
              {errorOutput !== undefined && (
                <ScriptEditor
                  className="text-sm text-red-600 mx-2.5 pb-2.5"
                  language="python"
                  code={errorOutput}
                  setCode={() => {}}
                  readonly
                  padding={0}
                />
              )}
            </ScriptSpecification>
          </div>

          {csv.data && scaledResults.length > 0 && (<>
            <div
              className={cn(
                "mb-6 transition-opacity ease-out duration-200 ",
                executionLoading && "opacity-50 " || "opacity-100"
              )}
            >
              <ScalingSummary
                userScalingSummaries={scaledResults}
                hasGradingSheet={!!gradingSheet}
              />
            </div>

            {(
              <div className="flex justify-center gap-4 flex-wrap p-4">
                {/* Display the grading sheet only to logged in users */}
                {isLoggedIn && (
                  <Button
                    className="bg-git-int-primary text-git-int-text hover:bg-git-int-primary-hover"
                    onClick={() => {
                      setDialogOpen(true);
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
              </div>
            )}
          </>)}
        </CardContent>
      </Card>

      <ScriptExecutionGradingSheetDialog
        open={dialogOpen}
        onOpenChange={setDialogOpen}
        handleSheetSubmit={handleSheetSubmit}
      />
    </div>
  );
};

export default ScriptExecution;
