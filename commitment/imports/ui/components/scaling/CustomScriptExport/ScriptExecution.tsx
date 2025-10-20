import React from 'react';
import { Code, FileText, Clock } from 'lucide-react';

import { Card, CardContent, CardHeader, CardTitle } from '@base/card';
import { Button } from '@base/button';
import { ExportHistoryItem } from './ExportHistory';

interface ScriptExecutionProps {
  history: ExportHistoryItem[];
  isLoading?: boolean;
}

export const ScriptExecution: React.FC<ScriptExecutionProps> = ({
  history,
  isLoading = false
}) => {
  const latestExport = history.length > 0 ? history[0] : null;

  return (
    <div className="space-y-6">
      {/* Main Script Execution Card */}
      <Card className="bg-git-bg-elevated dark:bg-git-bg-primary border-git-stroke-primary">
        <CardHeader className="bg-git-int-primary">
          <CardTitle className="text-git-int-text">Script Execution</CardTitle>
          <p className="text-sm text-git-int-text/90">
            Execute custom scripts using exported CSV data. This section is currently under development.
          </p>
        </CardHeader>
        <CardContent className="bg-git-bg-elevated dark:bg-git-bg-primary pt-6">
          <div className="text-center py-8">
            <Code className="h-12 w-12 mx-auto mb-4 text-git-text-secondary opacity-50" />
            <h3 className="text-lg font-medium text-git-text-primary mb-2">Script Execution </h3>
            <p className="text-git-text-secondary mb-6">
              This section will allow you to execute custom scripts using the exported CSV data.
              The functionality is currently being developed.
            </p>
            
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

      {/* CSV Endpoint Information */}
      <Card className="bg-git-bg-elevated dark:bg-git-bg-primary border-git-stroke-primary">
        <CardHeader className="bg-git-int-primary">
          <CardTitle className="text-git-int-text">CSV Data Access</CardTitle>
          <p className="text-sm text-git-int-text/90">
            Information about accessing exported CSV data for script execution
          </p>
        </CardHeader>
        <CardContent className="bg-git-bg-elevated dark:bg-git-bg-primary pt-6">
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
                  disabled={isLoading}
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
          </div>
        </CardContent>
      </Card>
    </div>
  );
};

export default ScriptExecution;
