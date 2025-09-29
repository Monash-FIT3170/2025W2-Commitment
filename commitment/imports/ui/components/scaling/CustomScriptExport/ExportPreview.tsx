import React from 'react';
import { Download, Eye, EyeOff } from 'lucide-react';

import { Card, CardContent, CardHeader, CardTitle } from '../../ui/card';
import { Button } from '../../ui/button';

import { Table, TableBody, TableCell, TableHead, TableHeader, TableRow } from '../../ui/table';
import { ScrollArea } from '../../ui/scroll-area';

export interface ExportData {
  headers: string[];
  rows: (string | number)[][];
  summary: {
    totalRows: number;
    dateRange: string;
    branch: string;
    metrics: string[];
  };
}

interface ExportPreviewProps {
  data: ExportData | null;
  onExport: () => void;
  onClose: () => void;
  isLoading?: boolean;
}

export const ExportPreview: React.FC<ExportPreviewProps> = ({
  data,
  onExport,
  onClose,
  isLoading = false
}) => {
  const [showRawData, setShowRawData] = React.useState(false);
  const [maxRows, setMaxRows] = React.useState(50);

  if (!data) {
    return null;
  }

  const displayRows = showRawData ? data.rows : data.rows.slice(0, maxRows);
  const hasMoreRows = data.rows.length > maxRows;

  const formatValue = (value: string | number) => {
    if (typeof value === 'number') {
      return value.toLocaleString();
    }
    return value;
  };

  const getFileSize = () => {
    // Rough estimate: each row ~100 bytes, headers ~50 bytes
    const estimatedSize = (data.rows.length * 100) + 50;
    if (estimatedSize < 1024) return `${estimatedSize} B`;
    if (estimatedSize < 1024 * 1024) return `${(estimatedSize / 1024).toFixed(1)} KB`;
    return `${(estimatedSize / (1024 * 1024)).toFixed(1)} MB`;
  };

  return (
    <Card className="w-full bg-git-bg-elevated border-git-stroke-primary">
      <CardHeader className="bg-git-int-primary">
        <div className="flex items-center justify-between">
          <CardTitle className="text-git-int-text">Export Preview</CardTitle>
          <div className="flex items-center gap-2">
            <Button
              variant="outline"
              size="sm"
              onClick={() => setShowRawData(!showRawData)}
              className="bg-git-int-primary text-git-int-text hover:bg-git-int-primary-hover border-git-stroke-primary"
            >
              {showRawData ? <EyeOff className="h-4 w-4 mr-2" /> : <Eye className="h-4 w-4 mr-2" />}
              {showRawData ? 'Hide Raw Data' : 'Show Raw Data'}
            </Button>
            <Button 
              variant="outline" 
              size="sm" 
              onClick={onClose}
              className="bg-git-int-primary text-git-int-text hover:bg-git-int-primary-hover border-git-stroke-primary"
            >
              Close
            </Button>
          </div>
        </div>
        <p className="text-sm text-git-text-secondary">
          Review your data before exporting to CSV
        </p>
      </CardHeader>
      <CardContent className="space-y-4 bg-git-bg-elevated pt-6">
        {/* Summary Information */}
        <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
          <div className="space-y-1">
            <p className="text-sm font-medium text-git-text-primary">Total Rows</p>
            <p className="text-2xl font-bold text-git-text-primary">{data.summary.totalRows.toLocaleString()}</p>
          </div>
          <div className="space-y-1">
            <p className="text-sm font-medium text-git-text-primary">Date Range</p>
            <p className="text-sm text-git-text-secondary">{data.summary.dateRange}</p>
          </div>
          <div className="space-y-1">
            <p className="text-sm font-medium text-git-text-primary">Branch</p>
            <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-git-int-primary text-git-int-text">
              {data.summary.branch}
            </span>
          </div>
          <div className="space-y-1">
            <p className="text-sm font-medium text-git-text-primary">Estimated Size</p>
            <p className="text-sm text-git-text-secondary">{getFileSize()}</p>
          </div>
        </div>

        {/* Metrics Summary */}
        <div className="space-y-2">
          <p className="text-sm font-medium text-git-text-primary">Selected Metrics</p>
          <div className="flex flex-wrap gap-2">
            {data.summary.metrics.map((metric) => (
              <span key={metric} className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-git-int-primary text-git-int-text border border-git-stroke-primary">
                {metric}
              </span>
            ))}
          </div>
        </div>

        {/* Data Table */}
        <div className="space-y-2">
          <div className="flex items-center justify-between">
            <p className="text-sm font-medium text-git-text-primary">Data Preview</p>
            {hasMoreRows && !showRawData && (
              <Button
                variant="ghost"
                size="sm"
                onClick={() => setMaxRows(prev => Math.min(prev + 50, data.rows.length))}
                className="bg-git-int-primary text-git-int-text hover:bg-git-int-primary-hover"
              >
                Show More ({data.rows.length - maxRows} more rows)
              </Button>
            )}
          </div>
          
          <ScrollArea className="h-96 w-full border-git-stroke-primary rounded-md">
            <Table>
              <TableHeader>
                <TableRow>
                  {data.headers.map((header, index) => (
                    <TableHead key={index} className="sticky top-0 bg-git-int-secondary text-git-text-primary">
                      {header}
                    </TableHead>
                  ))}
                </TableRow>
              </TableHeader>
              <TableBody>
                {displayRows.map((row, rowIndex) => (
                  <TableRow key={rowIndex} className="bg-git-int-primary hover:bg-git-int-primary-hover">
                    {row.map((cell, cellIndex) => (
                      <TableCell key={cellIndex} className="font-mono text-sm text-git-int-text">
                        {formatValue(cell)}
                      </TableCell>
                    ))}
                  </TableRow>
                ))}
              </TableBody>
            </Table>
          </ScrollArea>
          
          {!showRawData && hasMoreRows && (
            <p className="text-xs text-git-text-secondary text-center">
              Showing {maxRows} of {data.rows.length} rows. Enable "Show Raw Data" to see all rows.
            </p>
          )}
        </div>

        {/* Export Actions */}
        <div className="flex items-center justify-between pt-4 border-t border-git-stroke-primary">
          <div className="text-sm text-git-text-secondary">
            Ready to export {data.summary.totalRows.toLocaleString()} rows
          </div>
          <div className="flex gap-2">
            <Button 
              variant="outline" 
              onClick={onClose}
              className="bg-git-int-primary text-git-int-text hover:bg-git-int-primary-hover border-git-stroke-primary"
            >
              Cancel
            </Button>
            <Button 
              onClick={onExport} 
              disabled={isLoading}
              className="bg-git-int-primary text-git-int-text hover:bg-git-int-primary-hover"
            >
              <Download className="h-4 w-4 mr-2" />
              {isLoading ? 'Exporting...' : 'Export CSV'}
            </Button>
          </div>
        </div>
      </CardContent>
    </Card>
  );
};
