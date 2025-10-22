import React, { useState, useEffect } from 'react';
import { Download, Trash2, Clock, FileText } from 'lucide-react';
import { format } from 'date-fns';

import { Card, CardContent, CardHeader, CardTitle } from '@base/card';
import { Button } from '@base/button';

import { Table, TableBody, TableCell, TableHead, TableHeader, TableRow } from '@base/table';
import { ScrollArea } from '@base/scroll-area';

export interface ExportHistoryItem {
  id: string;
  filename: string;
  branch: string;
  dateRange: string;
  metrics: string[];
  rowCount: number;
  exportedAt: Date;
  fileSize: string;
}

interface ExportHistoryProps {
  history: ExportHistoryItem[];
  onReExport: (item: ExportHistoryItem) => void;
  onDelete: (id: string) => void;
  onClearHistory: () => void;
  isLoading?: boolean;
}

export const ExportHistory: React.FC<ExportHistoryProps> = ({
  history,
  onReExport,
  onDelete,
  onClearHistory,
  isLoading = false
}) => {
  const [sortBy, setSortBy] = useState<'date' | 'filename' | 'branch'>('date');
  const [sortOrder, setSortOrder] = useState<'asc' | 'desc'>('desc');

  const sortedHistory = React.useMemo(() => {
    return [...history].sort((a, b) => {
      let comparison = 0;
      
      switch (sortBy) {
        case 'date':
          comparison = a.exportedAt.getTime() - b.exportedAt.getTime();
          break;
        case 'filename':
          comparison = a.filename.localeCompare(b.filename);
          break;
        case 'branch':
          comparison = a.branch.localeCompare(b.branch);
          break;
      }
      
      return sortOrder === 'asc' ? comparison : -comparison;
    });
  }, [history, sortBy, sortOrder]);

  const handleSort = (column: 'date' | 'filename' | 'branch') => {
    if (sortBy === column) {
      setSortOrder(prev => prev === 'asc' ? 'desc' : 'asc');
    } else {
      setSortBy(column);
      setSortOrder('asc');
    }
  };

  const getSortIcon = (column: 'date' | 'filename' | 'branch') => {
    if (sortBy !== column) return null;
    return sortOrder === 'asc' ? '↑' : '↓';
  };

  if (history.length === 0) {
    return (
      <Card className="w-full bg-git-bg-elevated border-git-stroke-primary rounded-xl">
        <CardHeader className="bg-git-int-primary rounded-t-xl">
          <CardTitle className="text-git-int-text">Export History</CardTitle>
          <p className="text-sm text-git-int-text/90">
            Your previous exports will appear here
          </p>
        </CardHeader>
        <CardContent className="bg-git-bg-elevated pt-6">
          <div className="text-center py-8 text-git-text-secondary">
            <FileText className="h-12 w-12 mx-auto mb-4 opacity-50" />
            <p className="text-git-text-primary">No exports yet</p>
            <p className="text-sm text-git-text-secondary">Start by selecting data and creating your first export</p>
          </div>
        </CardContent>
      </Card>
    );
  }

  return (
    <Card className="w-full bg-git-bg-elevated dark:bg-git-bg-primary border-git-stroke-primary rounded-xl">
        <CardHeader className="bg-git-int-primary rounded-t-xl">
          <div className="flex items-center justify-between">
            <div>
              <CardTitle className="text-git-int-text">Export History</CardTitle>
              <p className="text-sm text-git-int-text/90">
                {history.length} export{history.length !== 1 ? 's' : ''} in total
              </p>
            </div>
          <Button
            variant="outline"
            size="sm"
            onClick={onClearHistory}
            disabled={isLoading}
            className="bg-git-int-primary text-git-int-text hover:bg-git-int-primary-hover border-git-stroke-primary"
          >
            <Trash2 className="h-4 w-4 mr-2" />
            Clear All
          </Button>
        </div>
      </CardHeader>
      <CardContent className="bg-git-bg-elevated dark:bg-git-bg-primary pt-6">
        <ScrollArea className="h-96 w-full">
          <Table className="rounded-lg">
            <TableHeader>
              <TableRow className="rounded-t-lg">
                <TableHead 
                  className="cursor-pointer hover:bg-git-int-secondary text-git-text-primary bg-git-int-secondary rounded-tl-lg"
                  onClick={() => handleSort('filename')}
                >
                  Filename {getSortIcon('filename')}
                </TableHead>
                <TableHead 
                  className="cursor-pointer hover:bg-git-int-secondary text-git-text-primary bg-git-int-secondary"
                  onClick={() => handleSort('branch')}
                >
                  Branch {getSortIcon('branch')}
                </TableHead>
                <TableHead className="text-git-text-primary bg-git-int-secondary">Date Range</TableHead>
                <TableHead className="text-git-text-primary bg-git-int-secondary">Metrics</TableHead>
                <TableHead className="text-git-text-primary bg-git-int-secondary">Size</TableHead>
                <TableHead 
                  className="cursor-pointer hover:bg-git-int-secondary text-git-text-primary bg-git-int-secondary"
                  onClick={() => handleSort('date')}
                >
                  Exported {getSortIcon('date')}
                </TableHead>
                <TableHead className="text-right text-git-text-primary bg-git-int-secondary rounded-tr-lg">Actions</TableHead>
              </TableRow>
            </TableHeader>
            <TableBody>
              {sortedHistory.map((item) => (
                <TableRow key={item.id} className="bg-git-int-primary hover:bg-git-int-primary rounded-lg">
                  <TableCell className="font-medium text-git-int-text rounded-l-lg">
                    <div className="flex items-center gap-2">
                      <FileText className="h-4 w-4 text-git-int-text" />
                      <span className="truncate max-w-[200px] text-git-int-text" title={item.filename}>
                        {item.filename}
                      </span>
                    </div>
                  </TableCell>
                  <TableCell>
                    <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-git-int-secondary text-white">
                      {item.branch}
                    </span>
                  </TableCell>
                  <TableCell className="text-sm text-white">
                    {item.dateRange}
                  </TableCell>
                  <TableCell>
                    <div className="flex flex-wrap gap-1">
                      {item.metrics.map((metric) => (
                        <span key={metric} className="inline-flex items-center px-2 py-1 rounded-full text-xs font-medium bg-git-int-secondary text-white border border-white">
                          {metric}
                        </span>
                      ))}
                    </div>
                  </TableCell>
                  <TableCell className="text-sm text-white">
                    {item.fileSize}
                  </TableCell>
                  <TableCell className="text-sm text-white">
                    <div className="flex items-center gap-1">
                      <Clock className="h-3 w-3 text-white" />
                      {format(item.exportedAt, 'MMM d, HH:mm')}
                    </div>
                  </TableCell>
                  <TableCell className="text-right rounded-r-lg">
                    <div className="flex items-center justify-end gap-2">
                      <Button
                        variant="ghost"
                        size="sm"
                        onClick={() => onReExport(item)}
                        disabled={isLoading}
                        className="text-git-int-text hover:bg-git-int-primary-hover"
                      >
                        <Download className="h-4 w-4 text-git-int-text" />
                      </Button>
                      <Button
                        variant="ghost"
                        size="sm"
                        onClick={() => onDelete(item.id)}
                        disabled={isLoading}
                        className="text-git-int-text hover:bg-git-int-primary-hover"
                      >
                        <Trash2 className="h-4 w-4 text-git-int-text" />
                      </Button>
                    </div>
                  </TableCell>
                </TableRow>
              ))}
            </TableBody>
          </Table>
        </ScrollArea>
      </CardContent>
    </Card>
  );
};

// Hook for managing export history
export const useExportHistory = () => {
  const [history, setHistory] = useState<ExportHistoryItem[]>([]);

  // Load history from localStorage on mount
  useEffect(() => {
    const savedHistory = localStorage.getItem('commitment-export-history');
    if (savedHistory) {
      try {
        const parsed = JSON.parse(savedHistory);
        // Convert date strings back to Date objects
        const historyWithDates = parsed.map((item: any) => ({
          ...item,
          exportedAt: new Date(item.exportedAt)
        }));
        setHistory(historyWithDates);
      } catch (error) {
        console.error('Error loading export history:', error);
      }
    }
  }, []);

  // Save history to localStorage whenever it changes
  useEffect(() => {
    localStorage.setItem('commitment-export-history', JSON.stringify(history));
  }, [history]);

  const addExport = (item: Omit<ExportHistoryItem, 'id' | 'exportedAt'>) => {
    const newItem: ExportHistoryItem = {
      ...item,
      id: `export-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`,
      exportedAt: new Date()
    };
    
    setHistory(prev => [newItem, ...prev]);
    return newItem;
  };

  const deleteExport = (id: string) => {
    setHistory(prev => prev.filter(item => item.id !== id));
  };

  const clearHistory = () => {
    setHistory([]);
  };

  const reExport = (item: ExportHistoryItem) => {
    // This would trigger a re-export with the same configuration
    console.log('Re-exporting:', item);
  };

  return {
    history,
    addExport,
    deleteExport,
    clearHistory,
    reExport
  };
};
