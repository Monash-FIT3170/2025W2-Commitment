import React, { useState, useEffect } from 'react';
import { type DateRange } from 'react-day-picker';
import { Meteor } from 'meteor/meteor';

import { Card, CardContent, CardHeader, CardTitle } from '../../ui/card';
import { Button } from '../../ui/button';
import { Label } from '../../ui/label';
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '../../ui/select';
import { Checkbox } from '../../ui/checkbox';
import { DropdownMenu, DropdownMenuCheckboxItem, DropdownMenuContent, DropdownMenuTrigger } from '../../ui/dropdown-menu';
import { DatePicker } from './DatePicker';

export interface MetricOption {
  id: string;
  label: string;
  description: string;
  category: 'commits' | 'code' | 'collaboration' | 'time';
}

export interface DataSelectionConfig {
  branch: string;
  dateRange: DateRange | undefined;
  selectedMetrics: string[];
  includeRawData: boolean;
  groupBy: 'contributor' | 'date' | 'none';
}

interface DataSelectionPanelProps {
  availableBranches: string[];
  repoUrl?: string;
  onConfigChange: (config: DataSelectionConfig) => void;
  onPreviewData: () => void;
  isLoading?: boolean;
}

const METRIC_OPTIONS: MetricOption[] = [
  // Commits category
  { id: 'total_commits', label: 'Total Commits', description: 'Total number of commits', category: 'commits' },
  { id: 'commit_frequency', label: 'Commit Frequency', description: 'Commits per day/week', category: 'commits' },
  { id: 'commit_size', label: 'Average Commit Size', description: 'Average lines changed per commit', category: 'commits' },
  
  // Code category
  { id: 'lines_added', label: 'Lines Added', description: 'Total lines of code added', category: 'code' },
  { id: 'lines_deleted', label: 'Lines Deleted', description: 'Total lines of code deleted', category: 'code' },
  { id: 'lines_modified', label: 'Lines Modified', description: 'Total lines of code modified', category: 'code' },
  { id: 'net_lines', label: 'Net Lines', description: 'Net change in lines of code', category: 'code' },
  { id: 'file_count', label: 'Files Changed', description: 'Number of files modified', category: 'code' },
  
  // Collaboration category
  { id: 'contributors', label: 'Contributors', description: 'Number of unique contributors', category: 'collaboration' },
  { id: 'collaboration_score', label: 'Collaboration Score', description: 'Measure of team collaboration', category: 'collaboration' },
  
  // Time category
  { id: 'active_days', label: 'Active Days', description: 'Days with commits', category: 'time' },
  { id: 'first_commit', label: 'First Commit', description: 'Date of first commit', category: 'time' },
  { id: 'last_commit', label: 'Last Commit', description: 'Date of last commit', category: 'time' },
];

export const DataSelectionPanel: React.FC<DataSelectionPanelProps> = ({
  availableBranches,
  repoUrl,
  onConfigChange,
  onPreviewData,
  isLoading = false
}) => {
  const [config, setConfig] = useState<DataSelectionConfig>({
    branch: '',
    dateRange: {
      from: new Date(Date.now() - 30 * 24 * 60 * 60 * 1000), // 30 days ago
      to: new Date()
    },
    selectedMetrics: ['total_commits', 'lines_added', 'lines_deleted'],
    includeRawData: true,
    groupBy: 'contributor'
  });

  // Fetch repository metadata to get actual date range
  useEffect(() => {
    if (!repoUrl) return;

    Meteor.call('repo.getMetadata', repoUrl, (error: any, metadata: any) => {
      if (error) {
        console.error('Error fetching repository metadata:', error);
        return;
      }

      if (metadata && metadata.dateRange) {
        console.log('Repository metadata:', metadata);
        setConfig(prev => ({
          ...prev,
          dateRange: metadata.dateRange,
          branch: metadata.branches && metadata.branches.length > 0 
            ? (metadata.branches.includes('main') ? 'main' : metadata.branches[0])
            : prev.branch
        }));
      }
    });
  }, [repoUrl]);

  // Update parent component when config changes
  useEffect(() => {
    onConfigChange(config);
  }, [config, onConfigChange]);

  const handleMetricToggle = (metricId: string) => {
    setConfig(prev => ({
      ...prev,
      selectedMetrics: prev.selectedMetrics.includes(metricId)
        ? prev.selectedMetrics.filter(id => id !== metricId)
        : [...prev.selectedMetrics, metricId]
    }));
  };

  const handleSelectAll = (category: string) => {
    const categoryMetrics = METRIC_OPTIONS
      .filter(metric => metric.category === category)
      .map(metric => metric.id);
    
    setConfig(prev => ({
      ...prev,
      selectedMetrics: [...new Set([...prev.selectedMetrics, ...categoryMetrics])]
    }));
  };

  const handleDeselectAll = (category: string) => {
    const categoryMetrics = METRIC_OPTIONS
      .filter(metric => metric.category === category)
      .map(metric => metric.id);
    
    setConfig(prev => ({
      ...prev,
      selectedMetrics: prev.selectedMetrics.filter(id => !categoryMetrics.includes(id))
    }));
  };

  const isConfigValid = config.branch && config.selectedMetrics.length > 0;

  return (
    <Card className="w-full bg-git-bg-elevated border-git-stroke-primary">
      <CardHeader className="bg-git-int-primary">
        <CardTitle className="text-git-int-text">Select Data for Export</CardTitle>
        <p className="text-sm text-git-int-text/90">
          Choose the branch, time period, and metrics you want to export for your custom scaling script.
        </p>
      </CardHeader>
      <CardContent className="space-y-6 bg-git-bg-elevated pt-6">
        {/* Branch Selection */}
        <div className="space-y-2">
          <Label htmlFor="branch" className="text-git-text-primary">Branch</Label>
          <DropdownMenu>
            <DropdownMenuTrigger asChild>
              <Button
                variant="outline"
                className="w-full justify-start text-left font-normal border-2 rounded-lg border-git-stroke-primary/40 text-git-text-primary"
              >
                {config.branch || 'Select a branch'}
              </Button>
            </DropdownMenuTrigger>
            <DropdownMenuContent align="start" sideOffset={4} className="w-[280px] focus:ring-0 border-2 border-git-stroke-primary/40">
              {availableBranches.map((branch) => (
                <DropdownMenuCheckboxItem
                  key={branch}
                  checked={config.branch === branch}
                  onCheckedChange={() => setConfig(prev => ({ ...prev, branch }))}
                >
                  {branch}
                </DropdownMenuCheckboxItem>
              ))}
            </DropdownMenuContent>
          </DropdownMenu>
        </div>

        {/* Date Range Selection */}
        <div className="space-y-2">
          <Label className="text-git-text-primary">Date Range</Label>
          <DatePicker
            defaultValue={config.dateRange}
            onChange={(dateRange) => setConfig(prev => ({ ...prev, dateRange }))}
          />
        </div>

        {/* Metrics Selection */}
        <div className="space-y-4">
          <div className="flex items-center justify-between">
            <Label className="text-git-text-primary">Metrics to Export</Label>
            <div className="flex gap-2">
              <Button
                variant="outline"
                size="sm"
                onClick={() => setConfig(prev => ({ ...prev, selectedMetrics: METRIC_OPTIONS.map(m => m.id) }))}
                className="bg-git-int-primary text-git-int-text hover:bg-git-int-primary-hover border-git-stroke-primary"
              >
                Select All
              </Button>
              <Button
                variant="outline"
                size="sm"
                onClick={() => setConfig(prev => ({ ...prev, selectedMetrics: [] }))}
                className="bg-git-int-primary text-git-int-text hover:bg-git-int-primary-hover border-git-stroke-primary"
              >
                Clear All
              </Button>
            </div>
          </div>

          {/* Group metrics by category */}
          {['commits', 'code', 'collaboration', 'time'].map((category) => {
            const categoryMetrics = METRIC_OPTIONS.filter(metric => metric.category === category);

            return (
              <div key={category} className="space-y-2">
                <div className="flex items-center justify-between">
                  <h4 className="text-sm font-medium text-git-text-secondary capitalize">{category} Metrics</h4>
                  <div className="flex gap-1">
                    <Button
                      variant="ghost"
                      size="sm"
                      onClick={() => handleSelectAll(category)}
                    >
                      All
                    </Button>
                    <Button
                      variant="ghost"
                      size="sm"
                      onClick={() => handleDeselectAll(category)}
                    >
                      None
                    </Button>
                  </div>
                </div>
                
                <div className="grid grid-cols-1 gap-2">
                  {categoryMetrics.map((metric) => (
                    <div key={metric.id} className="flex items-center space-x-2">
                      <Checkbox
                        id={metric.id}
                        checked={config.selectedMetrics.includes(metric.id)}
                        onCheckedChange={() => handleMetricToggle(metric.id)}
                      />
                      <div className="flex-1">
                        <Label
                          htmlFor={metric.id}
                          className="text-sm font-medium text-git-text-primary leading-none peer-disabled:cursor-not-allowed peer-disabled:opacity-70"
                        >
                          {metric.label}
                        </Label>
                        <p className="text-xs text-git-text-secondary mt-1">
                          {metric.description}
                        </p>
                      </div>
                    </div>
                  ))}
                </div>
              </div>
            );
          })}
        </div>

        {/* Export Options */}
        <div className="space-y-4">
          <Label className="text-git-text-primary">Export Options</Label>
          
          <div className="flex items-center space-x-2">
            <Checkbox
              id="includeRawData"
              checked={config.includeRawData}
              onCheckedChange={(checked) => 
                setConfig(prev => ({ ...prev, includeRawData: checked as boolean }))
              }
            />
            <Label htmlFor="includeRawData" className="text-sm text-git-text-primary">
              Include raw commit data (individual commits, not just aggregated metrics)
            </Label>
          </div>

          <div className="space-y-2">
            <Label htmlFor="groupBy" className="text-git-text-primary">Group By</Label>
             <Select
              value={config.groupBy}
              onValueChange={(value: 'contributor' | 'date' | 'none') => 
                setConfig(prev => ({ ...prev, groupBy: value }))
              }
            >
               <SelectTrigger className="text-git-text-primary">
                 <SelectValue placeholder="Group by" />
              </SelectTrigger>
              <SelectContent>
                <SelectItem value="contributor">Contributor</SelectItem>
                <SelectItem value="date">Date</SelectItem>
                <SelectItem value="none">No Grouping</SelectItem>
              </SelectContent>
            </Select>
          </div>
        </div>

        {/* Preview Button */}
        <div className="pt-4">
          <Button
            onClick={onPreviewData}
            disabled={!isConfigValid || isLoading}
            className="w-full bg-git-int-primary text-git-int-text hover:bg-git-int-primary-hover"
          >
            {isLoading ? 'Loading...' : 'Preview Data'}
          </Button>
        </div>
      </CardContent>
    </Card>
  );
};
