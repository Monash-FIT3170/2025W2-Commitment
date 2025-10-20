import React, {useState} from 'react';
import {Clock, Code, FileText} from 'lucide-react';

import {Card, CardContent, CardHeader, CardTitle} from '@base/card';
import {Button} from '@base/button';
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

interface ScriptExecutionProps extends DataSelectionPanelProps {
  history: ExportHistoryItem[];
  onDataRequest: (config: DataSelectionConfig) => Promise<ExportData>;
}

export const ScriptExecution: React.FC<ScriptExecutionProps> = ({
  history,
  onDataRequest,
  ...dataSelectionPanelProps
}: ScriptExecutionProps) => {
  const latestExport = history.length > 0 ? history[0] : null;

  const [code, setCode] = useLocalStorage('custom-execution-script', initialScript);
  const [currentConfig, setCurrentConfig] = useState<DataSelectionConfig | null>(null);

  const csv = useAsync<string | null, string>(async () => {
    if (currentConfig === null)
      return null;

    console.log(currentConfig);

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
    if (currentConfig === null || currentConfig === undefined)
      return;

    console.log("Executing...", code, currentConfig)
  }

  return (
    <div className="space-y-6">
      

      {/* Main Script Execution Card */}
      <Card className="bg-git-bg-elevated border-git-stroke-primary">
        <CardHeader className="bg-git-int-primary">
          <CardTitle className="text-git-int-text">Script Execution</CardTitle>
          <p className="text-sm text-git-int-text/90">
            Execute custom scripts using exported CSV data. This section is currently under development.
          </p>
        </CardHeader>
        <CardContent className="bg-git-bg-elevated pt-6">
          <div className="py-3">

            <ScriptSpecification
              className="mb-6"
              code={code}
              setCode={setCode}
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
              className="mb-6"
              code={csv.data ?? ""}
              setCode={() => {}}
              name={"data.csv"}
              icon={<FileText size="sm"/>}
              language="csv"
              readonly
            />
          </div>
        </CardContent>
      </Card>

      {/* CSV Endpoint Information */}
      <Card className="bg-git-bg-elevated border-git-stroke-primary">
        <CardHeader className="bg-git-int-primary">
          <CardTitle className="text-git-int-text">CSV Data Access</CardTitle>
          <p className="text-sm text-git-int-text/90">
            Information about accessing exported CSV data for script execution
          </p>
        </CardHeader>
        <CardContent className="bg-git-bg-elevated pt-6">
          <div className="space-y-4">
            <div>
              <h4 className="font-medium text-git-text-primary mb-2">Available Data</h4>
              <p className="text-sm text-git-text-secondary">
                CSV files are generated from the Data Selection and Preview tabs. Each export includes:
              </p>
              <ul className="mt-2 text-sm text-git-text-secondary list-disc list-inside space-y-1">
                <li>Contributor information and metrics</li>
                <li>Date range filtering based on your selection</li>
                <li>Selected metrics (commits, lines added/deleted, etc.)</li>
                <li>Collaboration scores calculated for the selected period</li>
              </ul>
            </div>

            <div>
              <h4 className="font-medium text-git-text-primary mb-2">Export History</h4>
              <p className="text-sm text-git-text-secondary">
                {history.length > 0 
                  ? `You have ${history.length} export${history.length !== 1 ? 's' : ''} available for script execution.`
                  : 'No exports available yet. Create an export in the Data Selection tab first.'
                }
              </p>
            </div>

            {history.length > 0 && (
              <div className="mt-4">
                <Button
                  variant="outline"
                  size="sm"
                  disabled={dataSelectionPanelProps.isLoading}
                  className="bg-git-int-primary text-git-int-text hover:bg-git-int-primary-hover border-git-stroke-primary"
                  onClick={() => {
                    // add logic to execute the script :)
                  }}
                >
                  <Code className="h-4 w-4 mr-2" />
                  Execute Script (Coming Soon)
                </Button>
              </div>
            )}

            {latestExport && (
              <div className="mt-6 p-4 bg-git-int-secondary rounded-lg border border-git-stroke-primary">
                <h4 className="font-medium text-git-text-primary mb-2">Latest Export Available</h4>
                <div className="flex items-center gap-2 text-sm text-git-text-secondary">
                  <FileText className="h-4 w-4" />
                  <span>{latestExport.filename}</span>
                  <Clock className="h-4 w-4 ml-2" />
                  <span>Exported {latestExport.exportedAt.toLocaleDateString()}</span>
                </div>
                <p className="text-xs text-git-text-secondary mt-1">
                  {latestExport.rowCount} rows • {latestExport.fileSize} • {latestExport.branch}
                </p>
              </div>
            )}
          </div>

        </CardContent>
      </Card>
    </div>
  );
};

export default ScriptExecution;
