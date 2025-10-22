import React, {useState} from 'react';
import {FileCode2, FileText, LoaderCircle, Terminal} from 'lucide-react';
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
import {PythonExecutorResponse, type UserScalingSummary} from "@api/types";
import {meteorCallAsync} from "@api/meteor_interface";
import ScalingSummary from "@ui/components/scaling/ScalingSummary";
import {cn} from "@ui/lib/utils";
import ScriptEditor from "@ui/components/scaling/CustomScriptExport/ScriptExecution/ScriptEditor";

interface ScriptExecutionProps extends DataSelectionPanelProps {
  history: ExportHistoryItem[];
  onDataRequest: (config: DataSelectionConfig) => Promise<ExportData>;
}

export const ScriptExecution: React.FC<ScriptExecutionProps> = ({
  history,
  onDataRequest,
  ...dataSelectionPanelProps
}: ScriptExecutionProps) => {
  // const latestExport = history.length > 0 ? history[0] : null;

  const [code, setCode] = useLocalStorage('custom-execution-script', initialScript);
  const [currentConfig, setCurrentConfig] = useState<DataSelectionConfig | null>(null);
  const [response, setResponse] = useState<PythonExecutorResponse>({});

  const [scaledResults, setScaledResults] = useState<UserScalingSummary[]>([]);
  const [executionLoading, setExecutionLoading] = useState<boolean>(false);

  // const responseDataJson = useMemo(() => {
  //   if (response.data)
  //     return JSON.stringify(response.data, null, 2);
  //   return "";
  // }, [response.data]);

  const csv = useAsync<string | null, string>(async () => {
    if (currentConfig === null)
      return null;

    try {
      const data = await onDataRequest(currentConfig);

      const csv_headers = data.headers.join(',');
      const csv_rows = data.rows.map(row => row.join(',')).join('\n');
      return `${csv_headers}\n${csv_rows}`;
    } catch (err) {
      throw (err instanceof Error ? err.message : 'Failed to load data');
    }
  }, [currentConfig])

  const onExecuteButton = () => {
    if (csv.data === null)
      return;

    // Call the API!
    setExecutionLoading(true);
    meteorCallAsync<PythonExecutorResponse>("pythonExecutor")(code, csv.data)
      .then(v => {
        setResponse(v);
        return v;
      })
      .then((response) => {
        // Get first row to be able to convert indexed values back to identifiers
        if (csv.data === null || response.data === undefined)
          return;
        const csvLines = csv.data
          .split('\n')
        if (csvLines.length < 2)
          return;
        const csvFirstCol = csvLines
          .slice(1)
          .map(row => row.split(',')[0]);

        // Turn response data into usable UserScalingSummary objects
        const filteredData = response.data
          .filter((row: unknown) => {
            // Ensure entries are arrays
            if (row === null || !Array.isArray(row))
              return false;
            // Ensure entries are of the right size
            if (row.length !== 2)
              return false;

            // Only accept arrays of [number, number] or [string, number]
            return !((typeof row[0] !== "string" || typeof row[1] !== "number") &&
                     (typeof row[0] !== "number" || typeof row[1] !== "number"));
          })
          .map(row => row as [number | string, number])
          .map((row: [number | string, number]) => {
            return {
              name: (typeof row[0] === "number") ? csvFirstCol[row[0]] : row[0],
              scale: row[1]
            } as UserScalingSummary;
          });

        setScaledResults(filteredData);
        return filteredData;
      })
      .catch((e) => {
        console.error("Caught error trying to use python executor: ", e);
        setScaledResults([]);
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
                "mb-6 mt-0 min-h-[45vh]",
                cn(
                  "mb-6 mt-0 min-h-[45vh] transition-opacity ease-out duration-200 ",
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
              code={response.stdout?.trimEnd() ?? ""}
              setCode={() => {}}
              name={"Execution Output"}
              icon={<Terminal size="sm"/>}
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


          {csv.data && scaledResults.length > 0 && (
            <div
              className={cn(
                "mb-6 transition-opacity ease-out duration-200 ",
                executionLoading && "opacity-50 " || "opacity-100"
              )}
            >
              <ScalingSummary
                userScalingSummaries={scaledResults}
                hasGradingSheet={false}
              />
            </div>
          )}

        </CardContent>
      </Card>

      {/* CSV Endpoint Information */}
    </div>
  );
};

// <Card className="bg-git-bg-elevated border-git-stroke-primary">
//   <CardHeader className="bg-git-int-primary">
//     <CardTitle className="text-git-int-text">CSV Data Access</CardTitle>
//     <p className="text-sm text-git-int-text/90">
//       Information about accessing exported CSV data for script execution
//     </p>
//   </CardHeader>
//   <CardContent className="bg-git-bg-elevated pt-6">
//     <div className="space-y-4">
//       <div>
//         <h4 className="font-medium text-git-text-primary mb-2">Available Data</h4>
//         <p className="text-sm text-git-text-secondary">
//           CSV files are generated from the Data Selection and Preview tabs. Each export includes:
//         </p>
//         <ul className="mt-2 text-sm text-git-text-secondary list-disc list-inside space-y-1">
//           <li>Contributor information and metrics</li>
//           <li>Date range filtering based on your selection</li>
//           <li>Selected metrics (commits, lines added/deleted, etc.)</li>
//           <li>Collaboration scores calculated for the selected period</li>
//         </ul>
//       </div>
//
//       <div>
//         <h4 className="font-medium text-git-text-primary mb-2">Export History</h4>
//         <p className="text-sm text-git-text-secondary">
//           {history.length > 0
//             ? `You have ${history.length} export${history.length !== 1 ? 's' : ''} available for script execution.`
//             : 'No exports available yet. Create an export in the Data Selection tab first.'
//           }
//         </p>
//       </div>
//
//       {history.length > 0 && (
//         <div className="mt-4">
//           <Button
//             variant="outline"
//             size="sm"
//             disabled={dataSelectionPanelProps.isLoading}
//             className="bg-git-int-primary text-git-int-text hover:bg-git-int-primary-hover border-git-stroke-primary"
//             onClick={() => {
//               // add logic to execute the script :)
//             }}
//           >
//             <Code className="h-4 w-4 mr-2" />
//             Execute Script (Coming Soon)
//           </Button>
//         </div>
//       )}
//
//       {latestExport && (
//         <div className="mt-6 p-4 bg-git-int-secondary rounded-lg border border-git-stroke-primary">
//           <h4 className="font-medium text-git-text-primary mb-2">Latest Export Available</h4>
//           <div className="flex items-center gap-2 text-sm text-git-text-secondary">
//             <FileText className="h-4 w-4" />
//             <span>{latestExport.filename}</span>
//             <Clock className="h-4 w-4 ml-2" />
//             <span>Exported {latestExport.exportedAt.toLocaleDateString()}</span>
//           </div>
//           <p className="text-xs text-git-text-secondary mt-1">
//             {latestExport.rowCount} rows • {latestExport.fileSize} • {latestExport.branch}
//           </p>
//         </div>
//       )}
//     </div>
//
//   </CardContent>
// </Card>

export default ScriptExecution;
