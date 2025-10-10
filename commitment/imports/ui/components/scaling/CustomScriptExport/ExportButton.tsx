import React from 'react';
import { Download, FileText } from 'lucide-react';

import { Button } from '../../ui/button';

interface ExportButtonProps {
  onExport: () => void;
  isLoading?: boolean;
  disabled?: boolean;
  variant?: 'default' | 'outline' | 'secondary' | 'ghost' | 'link' | 'destructive';
  size?: 'default' | 'sm' | 'lg' | 'icon';
  className?: string;
  children?: React.ReactNode;
}

export const ExportButton: React.FC<ExportButtonProps> = ({
  onExport,
  isLoading = false,
  disabled = false,
  variant = 'default',
  size = 'default',
  className = '',
  children
}) => {
  return (
    <Button
      onClick={onExport}
      disabled={disabled || isLoading}
      variant={variant}
      size={size}
      className={className}
    >
      {isLoading ? (
        <>
          <FileText className="h-4 w-4 mr-2 animate-spin" />
          Exporting...
        </>
      ) : (
        <>
          <Download className="h-4 w-4 mr-2" />
          {children || 'Export CSV'}
        </>
      )}
    </Button>
  );
};

// Utility function to generate CSV content
export const generateCSV = (data: {
  headers: string[];
  rows: (string | number)[][];
}): string => {
  const { headers, rows } = data;
  
  // Escape CSV values
  const escapeCSVValue = (value: string | number): string => {
    const stringValue = String(value);
    
    // If the value contains comma, newline, or quote, wrap in quotes and escape internal quotes
    if (stringValue.includes(',') || stringValue.includes('\n') || stringValue.includes('"')) {
      return `"${stringValue.replace(/"/g, '""')}"`;
    }
    
    return stringValue;
  };

  // Create CSV content
  const csvRows = [
    // Headers
    headers.map(escapeCSVValue).join(','),
    // Data rows
    ...rows.map(row => row.map(escapeCSVValue).join(','))
  ];

  return csvRows.join('\n');
};

// Utility function to download CSV file
export const downloadCSV = (csvContent: string, filename: string): void => {
  // Create blob with proper MIME type
  const blob = new Blob([csvContent], { type: 'text/csv;charset=utf-8;' });
  
  // Create download link
  const link = document.createElement('a');
  const url = URL.createObjectURL(blob);
  
  link.setAttribute('href', url);
  link.setAttribute('download', filename);
  link.style.visibility = 'hidden';
  
  // Trigger download
  document.body.appendChild(link);
  link.click();
  document.body.removeChild(link);
  
  // Clean up
  URL.revokeObjectURL(url);
};

// Utility function to generate filename with timestamp
export const generateFilename = (branch: string, startDate: Date, endDate: Date): string => {
  const formatDate = (date: Date) => {
    return date.toISOString().split('T')[0]; // YYYY-MM-DD format
  };
  
  const start = formatDate(startDate);
  const end = formatDate(endDate);
  const timestamp = new Date().toISOString().split('T')[0];
  
  return `commitment-data_${branch}_${start}_to_${end}_${timestamp}.csv`;
};

// Hook for CSV export functionality
export const useCSVExport = () => {
  const [isExporting, setIsExporting] = React.useState(false);

  const exportToCSV = React.useCallback(async (
    data: { headers: string[]; rows: (string | number)[][] },
    filename: string
  ) => {
    setIsExporting(true);
    
    try {
      // Generate CSV content
      const csvContent = generateCSV(data);
      
      // Download the file
      downloadCSV(csvContent, filename);
      
      // Simulate a small delay for better UX
      await new Promise(resolve => setTimeout(resolve, 500));
      
    } catch (error) {
      console.error('Error exporting CSV:', error);
      throw error;
    } finally {
      setIsExporting(false);
    }
  }, []);

  return {
    exportToCSV,
    isExporting
  };
};
