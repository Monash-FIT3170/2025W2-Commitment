import React, { useState } from 'react';
import { InfoIcon, ChevronDown, ChevronUp } from 'lucide-react';

import { Card, CardContent, CardHeader, CardTitle } from '@base/card';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@base/tabs';
import { Alert, AlertDescription } from '@base/alert';
import { Button } from '@base/button';

import { DataSelectionPanel, DataSelectionConfig } from './DataSelectionPanel';
import { ExportPreview, ExportData } from './ExportPreview';
import { ExportHistory, useExportHistory, ExportHistoryItem } from './ExportHistory';
import { ScriptExecution } from './ScriptExecution';
import { useCSVExport, generateFilename, generateCSV } from './ExportButton';

interface CustomScriptExportProps {
  availableBranches: string[];
  repoUrl: string;
  onDataRequest: (config: DataSelectionConfig) => Promise<ExportData>;
  className?: string;
}

export const CustomScriptExport: React.FC<CustomScriptExportProps> = ({
  availableBranches,
  repoUrl,
  onDataRequest,
  className = ''
}) => {
  const [currentConfig, setCurrentConfig] = useState<DataSelectionConfig | null>(null);
  const [previewData, setPreviewData] = useState<ExportData | null>(null);
  const [isLoading, setIsLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [activeTab, setActiveTab] = useState('export');
  const [isHelpExpanded, setIsHelpExpanded] = useState(false);

  const { history, addExport, deleteExport, clearHistory } = useExportHistory();
  const { exportToCSV, isExporting } = useCSVExport();

  // Handle configuration changes
  const handleConfigChange = (config: DataSelectionConfig) => {
    setCurrentConfig(config);
    setError(null);
    // Don't automatically clear preview data on config changes
    // Let the user explicitly request new preview data
  };

  // Handle preview data request
  const handlePreviewData = () => {
    if (!currentConfig) return;

    setIsLoading(true);
    setError(null);
    // Clear previous preview data to ensure fresh data
    setPreviewData(null);

    const fetchData = async () => {
      try {
        const data = await onDataRequest(currentConfig);
        setPreviewData(data);
        setActiveTab('preview');
      } catch (err) {
        setError(err instanceof Error ? err.message : 'Failed to load data');
      } finally {
        setIsLoading(false);
      }
    };

    void fetchData();
  };

  // Handle CSV export
  const handleExport = () => {
    if (!currentConfig) return;

    const exportData = async () => {
      try {
        // Basic guards
        if (!currentConfig.branch || currentConfig.selectedMetrics.length === 0) {
          setError('Please select a branch and at least one metric.');
          setActiveTab('export');
          return;
        }

        // If we don't have preview data yet, fetch it first
        let dataToExport = previewData;
        if (!dataToExport) {
          setIsLoading(true);
          try {
            dataToExport = await onDataRequest(currentConfig);
            setPreviewData(dataToExport);
          } catch (err) {
            setError(err instanceof Error ? err.message : 'Failed to load data');
            return;
          } finally {
            setIsLoading(false);
          }
        }

        // Extract repo name from repoUrl
        const repoName = repoUrl.split('/').pop()?.replace('.git', '') || 'repository';
        const filename = generateFilename(repoName, currentConfig.branch);

        await exportToCSV(dataToExport, filename);

        // compute a more accurate file size for the history entry
        const csvForSize = generateCSV({ headers: dataToExport.headers, rows: dataToExport.rows });
        const bytes = new Blob([csvForSize]).size;
        const kb = bytes / 1024;
        const sizeLabel = kb < 1024 ? `${kb.toFixed(2)} KB` : `${(kb / 1024).toFixed(2)} MB`;

        // Add to history
        const historyItem: Omit<ExportHistoryItem, 'id' | 'exportedAt'> = {
          filename,
          branch: currentConfig.branch,
          dateRange: currentConfig.dateRange?.from && currentConfig.dateRange?.to 
            ? `${currentConfig.dateRange.from.toLocaleDateString()} - ${currentConfig.dateRange.to.toLocaleDateString()}`
            : 'No date range',
          metrics: currentConfig.selectedMetrics,
          rowCount: dataToExport.summary.totalRows,
          fileSize: sizeLabel
        };

        addExport(historyItem);

        // Return to history tab to show success context
        setActiveTab('history');
      } catch (err) {
        setError(err instanceof Error ? err.message : 'Failed to export CSV');
      }
    };

    void exportData();
  };

  // Handle re-export from history
  const handleReExport = (_item: ExportHistoryItem) => {
    // For re-export, we would need to reconstruct the config from the history item
    // This is a simplified version - in practice, you might want to store more config details
    setError('Re-export functionality requires additional configuration storage');
  };

  // Handle close preview
  const handleClosePreview = () => {
    setPreviewData(null);
    setActiveTab('export');
  };

  return (
    <div className={`space-y-6 ${className}`}>

      {/* Help Section - collapsible */}
      <Card className="bg-git-bg-elevated border-git-stroke-primary rounded-lg">
        <CardHeader className="bg-git-int-primary">
          <div className="flex items-center justify-between">
            <CardTitle className="text-lg text-git-int-text">How to Use</CardTitle>
            <Button
              variant="ghost"
              size="sm"
              onClick={() => setIsHelpExpanded(!isHelpExpanded)}
              className="text-git-int-text hover:bg-git-int-primary-hover p-1"
            >
              {isHelpExpanded ? (
                <ChevronUp className="h-4 w-4" />
              ) : (
                <ChevronDown className="h-4 w-4" />
              )}
            </Button>
          </div>
        </CardHeader>
        {isHelpExpanded && (
          <CardContent className="space-y-4 bg-git-bg-elevated pt-6">
            <div className="grid md:grid-cols-3 gap-4 text-sm">
              <div>
                <h4 className="font-medium mb-2 text-git-text-primary">1. Select Data</h4>
                <p className="text-git-text-secondary">
                  Choose the branch, time period, and metrics you want to export. 
                  You can include raw commit data or just aggregated metrics.
                </p>
              </div>
              <div>
                <h4 className="font-medium mb-2 text-git-text-primary">2. Preview & Export</h4>
                <p className="text-git-text-secondary">
                  Review your data in the preview table, then export as CSV. Format will be provided as repo_name-branch_name-timestamp.csv
                  The file will be downloaded to your computer.
                </p>
              </div>
              <div>
                <h4 className="font-medium mb-2 text-git-text-primary">3. Use in Scripts</h4>
                <p className="text-git-text-secondary">
                  Import the CSV into your preferred tool (Python, R, Excel, etc.) 
                  to create custom scaling algorithms.
                </p>
              </div>
            </div>
            
            <div className="pt-4 border-t border-git-stroke-primary">
              <h4 className="font-medium mb-2 text-git-text-primary">CSV Format</h4>
              <p className="text-sm text-git-text-secondary">
                The exported CSV includes headers and data rows. Each row represents either a contributor 
                (if grouped by contributor) or a time period (if grouped by date). Raw commit data 
                includes individual commit details when enabled.
              </p>
            </div>
          </CardContent>
        )}
      </Card>

      {/* Error Alert */}
      {error && (
        <Alert variant="destructive">
          <InfoIcon className="h-4 w-4" />
          <AlertDescription>{error}</AlertDescription>
        </Alert>
      )}

      {/* Main Content */}
      <Tabs value={activeTab} onValueChange={setActiveTab} className="w-full">
        <TabsList className="grid w-full grid-cols-4 bg-git-int-secondary">
          <TabsTrigger 
            value="export"
            className="data-[state=active]:bg-git-int-primary data-[state=active]:text-git-int-text text-git-text-primary"
          >
            Data Selection
          </TabsTrigger>
          <TabsTrigger 
            value="preview" 
            disabled={!previewData}
            className="data-[state=active]:bg-git-int-primary data-[state=active]:text-git-int-text text-git-text-primary disabled:text-git-text-secondary"
          >
            Preview
          </TabsTrigger>
          <TabsTrigger 
            value="history"
            className="data-[state=active]:bg-git-int-primary data-[state=active]:text-git-int-text text-git-text-primary"
          >
            History
          </TabsTrigger>
          <TabsTrigger 
            value="script-execution"
            className="data-[state=active]:bg-git-int-primary data-[state=active]:text-git-int-text text-git-text-primary"
          >
            Script Execution
          </TabsTrigger>
        </TabsList>

        {/* Data Selection Tab */}
        <TabsContent value="export" className="space-y-6">
            <DataSelectionPanel
              availableBranches={availableBranches}
              repoUrl={repoUrl}
              onConfigChange={handleConfigChange}
              onPreviewData={handlePreviewData}
              isLoading={isLoading}
            />
        </TabsContent>

        {/* Preview Tab */}
        <TabsContent value="preview" className="space-y-6">
          {previewData && (
            <ExportPreview
              data={previewData}
              onExport={handleExport}
              onClose={handleClosePreview}
              isLoading={isExporting}
            />
          )}
        </TabsContent>

        {/* History Tab */}
        <TabsContent value="history" className="space-y-6">
          <ExportHistory
            history={history}
            onReExport={handleReExport}
            onDelete={deleteExport}
            onClearHistory={clearHistory}
            isLoading={isLoading}
          />
        </TabsContent>

        {/* Script Execution Tab */}
        <TabsContent value="script-execution" className="space-y-6">
          <ScriptExecution
            history={history}
            isLoading={isLoading}
          />
        </TabsContent>
      </Tabs>

    </div>
  );
};

export default CustomScriptExport;
