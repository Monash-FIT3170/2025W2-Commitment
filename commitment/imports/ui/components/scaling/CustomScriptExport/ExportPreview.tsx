import React from 'react';
import { Download } from 'lucide-react';

import { Card, CardContent, CardHeader, CardTitle } from '@base/card';
import { Button } from '@base/button';

import { Table, TableBody, TableCell, TableHead, TableHeader, TableRow } from '@base/table';
import { ScrollArea } from '@base/scroll-area';

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
  if (!data) {
    return null;
  }

  const displayRows = data.rows;

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
    <Card className="w-full bg-git-bg-elevated dark:bg-git-bg-primary border-git-stroke-primary">
      <CardHeader className="bg-git-int-primary">
        <div className="flex items-center justify-between">
          <CardTitle className="text-git-int-text">Export Preview</CardTitle>
          <div className="flex items-center gap-2">
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
      <CardContent className="space-y-4 bg-git-bg-elevated dark:bg-git-bg-primary pt-6">
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
            <div className="flex items-center space-x-2">
              <p className="text-sm font-medium text-git-text-primary">Data Preview</p>
              <span className="text-xs text-git-text-secondary bg-git-int-secondary px-2 py-1 rounded">
                {data.headers.length} columns × {data.rows.length} rows
              </span>
            </div>
          </div>
          
          <div className="w-full border border-git-stroke-primary rounded-md overflow-auto max-h-96">
            <Table className="min-w-full">
              <TableHeader>
                <TableRow>
                  {data.headers.map((header, index) => (
                    <TableHead 
                      key={index} 
                      className="sticky top-0 bg-git-int-secondary text-git-text-primary whitespace-nowrap px-3 py-2 text-left font-medium"
                    >
                      {header}
                    </TableHead>
                  ))}
                </TableRow>
              </TableHeader>
              <TableBody>
                {displayRows.map((row, rowIndex) => (
                  <TableRow key={rowIndex} className="bg-git-int-primary hover:bg-git-int-primary-hover">
                    {row.map((cell, cellIndex) => (
                      <TableCell 
                        key={cellIndex} 
                        className="font-mono text-sm text-git-int-text whitespace-nowrap px-3 py-2"
                      >
                        {formatValue(cell)}
                      </TableCell>
                    ))}
                  </TableRow>
                ))}
              </TableBody>
            </Table>
          </div>
          
          {data.headers.length > 8 && (
            <p className="text-xs text-git-text-secondary text-center">
              💡 Scroll horizontally to see all {data.headers.length} columns
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
