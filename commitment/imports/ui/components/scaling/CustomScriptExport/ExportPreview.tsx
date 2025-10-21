import React from 'react';
import { Download } from 'lucide-react';

import { Card, CardContent, CardHeader, CardTitle } from '@base/card';
import { Button } from '@base/button';

import { Table, TableBody, TableCell, TableHead, TableHeader, TableRow } from '@base/table';
import { HighlightCardWithGraph } from '../../metrics/HighlightCard';

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
    // Calculate actual CSV content size
    const escapeCSVValue = (value: string | number): string => {
      const stringValue = String(value);
      
      // If the value contains comma, newline, or quote, wrap in quotes and escape internal quotes
      if (stringValue.includes(',') || stringValue.includes('\n') || stringValue.includes('"')) {
        return `"${stringValue.replace(/"/g, '""')}"`;
      }
      
      return stringValue;
    };

    // Calculate headers size
    const headersRow = data.headers.map(escapeCSVValue).join(',');
    
    // Calculate data rows size
    const dataRows = data.rows.map(row => row.map(escapeCSVValue).join(','));
    
    // Total CSV content
    const csvContent = [headersRow, ...dataRows].join('\n');
    
    // Get actual byte size (UTF-8 encoding)
    const actualSize = new Blob([csvContent], { type: 'text/csv;charset=utf-8;' }).size;
    
    if (actualSize < 1024) return `${actualSize} B`;
    if (actualSize < 1024 * 1024) return `${(actualSize / 1024).toFixed(1)} KB`;
    return `${(actualSize / (1024 * 1024)).toFixed(1)} MB`;
  };

  return (
    <Card className="w-full bg-git-bg-elevated dark:bg-git-bg-primary border-git-stroke-primary rounded-xl">
      <CardHeader className="bg-git-int-primary rounded-t-xl">
        <div className="flex items-center justify-between">
          <CardTitle className="text-git-int-text">Export Preview</CardTitle>
        </div>
        <p className="text-sm text-git-int-text/90">
          Review your data before exporting to CSV
        </p>
      </CardHeader>
      <CardContent className="space-y-4 bg-git-bg-elevated dark:bg-git-bg-primary pt-6">
        {/* Summary Information */}
              <div className="grid   grid-cols-1 md:grid-cols-2 lg:grid-cols-4 xl:grid-cols-4 gap-4 text-2xl"> 

        <HighlightCardWithGraph title={"Total Rows"} value={data.summary.totalRows.toLocaleString()}/>
        <HighlightCardWithGraph title={"Date Range"} value={data.summary.dateRange}/>
        <HighlightCardWithGraph title={"Branch"} value={data.summary.branch}/>
        <HighlightCardWithGraph title={"Estimated Size"} value={getFileSize()}/>
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
          
          <div className="w-full border border-git-stroke-primary rounded-xl overflow-auto max-h-96">
            <Table className="min-w-full">
              <TableHeader>
                <TableRow className="bg-git-int-primary hover:bg-git-int-primary-hover">
                  {data.headers.map((header, index) => (
                    <TableHead 
                      key={index} 
                      className="sticky top-0 text-git-int-text whitespace-nowrap px-3 py-2 text-left font-bold"
                    >
                      {header}
                    </TableHead>
                  ))}
                </TableRow>
              </TableHeader>
              <TableBody>
                {displayRows.map((row, rowIndex) => (
                  <TableRow key={rowIndex} className="bg-git-int-primary hover:bg-git-int-primary-hover">
                    {row.map((cell, cellIndex) => {
                      const isEmailCol = data.headers[cellIndex] === 'contributor_email';
                      if (isEmailCol) {
                        const emails = String(cell)
                          .split(',')
                          .map((e) => e.trim())
                          .filter((e) => e.length > 0);
                        return (
                          <TableCell key={cellIndex} className="text-sm text-git-int-text px-3 py-2 align-top">
                            <div className="flex flex-wrap gap-1">
                              {emails.map((email, idx) => (
                                <span
                                  key={`${email}-${idx}`}
                                  className="inline-flex items-center px-2 py-0.5 rounded-full text-xs font-medium bg-git-int-secondary text-white border border-white"
                                >
                                  {email}
                                </span>
                              ))}
                            </div>
                          </TableCell>
                        );
                      }
                      return (
                        <TableCell
                          key={cellIndex}
                          className="font-mono text-sm text-git-int-text whitespace-nowrap px-3 py-2"
                        >
                          {formatValue(cell)}
                        </TableCell>
                      );
                    })}
                  </TableRow>
                ))}
              </TableBody>
            </Table>
          </div>

          
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
              className="border border-git-stroke-primary/40 rounded-xl hover:bg-git-int-primary hover:text-git-int-text"
            >
              Cancel
            </Button>
            <Button 
              variant="outline"
              onClick={onExport} 
              disabled={isLoading}
              className="border border-git-stroke-primary/40 rounded-xl hover:bg-git-int-primary hover:text-git-int-text"
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
