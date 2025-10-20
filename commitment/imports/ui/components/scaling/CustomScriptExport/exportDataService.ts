import { DataSelectionConfig } from './DataSelectionPanel';
import { ExportData } from './ExportPreview';
import { meteorCallAsync } from '../../../../api/meteor_interface';
import { 
  FilteredData, 
  SerializableRepoData
} from '../../../../api/types';

// Real data service using actual API calls
export class ExportDataService {
  private static instance: ExportDataService;
  
  public static getInstance(): ExportDataService {
    if (!ExportDataService.instance) {
      ExportDataService.instance = new ExportDataService();
    }
    return ExportDataService.instance;
  }

  /**
   * Fetch data based on the provided configuration
   */
  async fetchExportData(config: DataSelectionConfig, repoUrl: string): Promise<ExportData> {
    try {
      console.log('ExportDataService: Fetching FRESH data for repoUrl:', repoUrl, 'at', new Date().toISOString());
      console.log('ExportDataService: Config:', config);
      console.log('ExportDataService: Date range from:', config.dateRange?.from);
      console.log('ExportDataService: Date range to:', config.dateRange?.to);
      console.log('ExportDataService: Branch:', config.branch);
      
      // Get filtered repository data from the server
      const filteredData: FilteredData = await meteorCallAsync<FilteredData>("repo.getFilteredData")({
        repoUrl,
        startDate: config.dateRange?.from || new Date(),
        endDate: config.dateRange?.to || new Date(),
        branch: config.branch,
        contributor: undefined, // Get all contributors when undefined
      });

      console.log('ExportDataService: Filtered data:', filteredData);
      console.log('ExportDataService: Repository data commits count:', filteredData.repositoryData?.allCommits?.length || 0);
      console.log('ExportDataService: Repository data contributors count:', filteredData.repositoryData?.contributors?.length || 0);

      // Metrics for the filtered data
      const allCommits = filteredData.repositoryData?.allCommits || [];
      if (allCommits.length > 0) {
        const commitDates = allCommits.map((commit: any) => commit.value.timestamp);
        const minDate = new Date(Math.min(...commitDates.map((d: any) => new Date(d).getTime())));
        const maxDate = new Date(Math.max(...commitDates.map((d: any) => new Date(d).getTime())));
        console.log('ExportDataService: Actual commit date range:', minDate.toISOString(), 'to', maxDate.toISOString());
      }

      // Calculate scaling results based on the filtered data for the selected date range
      let scalingResults: any[] = [];
      try {
        // Calculate scaling results directly from the filtered data
        scalingResults = this.calculateScalingResultsFromFilteredData(filteredData);
        console.log('ExportDataService: Scaling results for date range:', scalingResults);
        console.log('ExportDataService: Scaling results names:', scalingResults.map(r => r.name));
        console.log('ExportDataService: Scaling results scales:', scalingResults.map(r => r.scale));
      } catch (error) {
        console.warn('ExportDataService: Could not calculate scaling results, using fallback collaboration scores:', error);
      }

      // Generate export data from the real repository data
      const exportData = this.generateExportDataFromRepoDirect(config, filteredData, scalingResults);
      
      console.log('ExportDataService: Generated export data:', exportData);
      return exportData;
    } catch (error) {
      console.error('Error fetching export data:', error);
      throw new Error(`Failed to fetch repository data: ${error instanceof Error ? error.message : 'Unknown error'}`);
    }
  }

  /**
   * Generate export data directly from repository data 
   */
  private generateExportDataFromRepoDirect(
    config: DataSelectionConfig, 
    filteredData: FilteredData, 
    scalingResults: any[] = []
  ): ExportData {
    const { branch, dateRange, selectedMetrics, groupBy } = config;
    const startDate = dateRange?.from || new Date();
    const endDate = dateRange?.to || new Date();
    const { repositoryData } = filteredData;
    
    // Get contributors from the repository data
    const contributors = this.getContributorsFromRepoData(repositoryData);
    console.log('ExportDataService: Extracted contributors for direct export:', contributors);
    
    // Generate headers based on selected metrics and grouping
    const headers = this.generateHeaders(selectedMetrics, groupBy);
    console.log('ExportDataService: Generated headers:', headers);
    
    // Generate rows based on grouping
    const rows = this.generateRowsFromRepoDataDirect(
      selectedMetrics, 
      groupBy, 
      contributors, 
      repositoryData,
      startDate,
      endDate,
      scalingResults
    );
    console.log('ExportDataService: Generated rows count:', rows.length);
    console.log('ExportDataService: Generated rows:', rows);

    return {
      headers,
      rows,
      summary: {
        totalRows: rows.length,
        dateRange: `${startDate.toLocaleDateString()} - ${endDate.toLocaleDateString()}`,
        branch,
        metrics: selectedMetrics
      }
    };
  }


  /**
   * Extract contributors from repository data
   */
  private getContributorsFromRepoData(repoData: SerializableRepoData): string[] {
    console.log('ExportDataService: Getting contributors from repo data:', repoData);
    console.log('ExportDataService: Contributors array:', repoData.contributors);
    
    if (!repoData.contributors || !Array.isArray(repoData.contributors)) {
      console.warn('ExportDataService: No contributors array found in repo data');
      return [];
    }
    
    const contributors = repoData.contributors.map((contributor: any) => contributor.key);
    console.log('ExportDataService: Extracted contributors:', contributors);
    return contributors;
  }

  private generateHeaders(metrics: string[], groupBy: string): string[] {
    const headers: string[] = [];

    // Add grouping columns
    if (groupBy === 'contributor') {
      headers.push('contributor_email', 'contributor_name');
    } else if (groupBy === 'date') {
      headers.push('date');
    }

    // Add metric columns
    metrics.forEach(metric => {
      headers.push(this.getMetricHeader(metric));
    });

    return headers;
  }

  private getMetricHeader(metric: string): string {
    const metricHeaders: Record<string, string> = {
      'total_commits': 'total_commits',
      'commit_frequency': 'commits_per_day',
      'commit_size': 'avg_commit_size',
      'lines_added': 'lines_added',
      'lines_deleted': 'lines_deleted',
      'lines_modified': 'lines_modified',
      'net_lines': 'net_lines',
      'file_count': 'files_changed',
      'collaboration_score': 'collaboration_score',
      'active_days': 'active_days',
      'first_commit': 'first_commit_date',
      'last_commit': 'last_commit_date'
    };

    return metricHeaders[metric] || metric;
  }

  private generateRowsFromRepoDataDirect(
    metrics: string[], 
    groupBy: string, 
    contributors: string[], 
    repoData: SerializableRepoData,
    startDate: Date,
    endDate: Date,
    scalingResults: any[] = []
  ): (string | number)[][] {
    const rows: (string | number)[][] = [];

    if (groupBy === 'contributor') {
      // Generate one row per contributor
      contributors.forEach(contributorKey => {
        const row: (string | number)[] = [];
        
        // Get the contributor's display name for the output
        const contributorData = repoData.contributors.find(c => c.key === contributorKey);
        const contributorName = contributorData ? contributorData.value.name : contributorKey;
        const contributorEmails: string[] = contributorData?.value?.emails || [];
        const emailsString = contributorEmails.length > 0 ? contributorEmails.join(', ') : '';
        
        // Contributor info (match header order: contributor_email, contributor_name)
        row.push(emailsString);
        row.push(contributorName);
        
        // Get metrics for this contributor directly from repository data
        metrics.forEach(metric => {
          if (metric === 'collaboration_score') {
            // Use scaling results for collaboration score - match by contributor key
            let scalingResult = scalingResults.find(result => result.name === contributorKey);
            
            // If exact match fails, try case-insensitive match
            if (!scalingResult) {
              scalingResult = scalingResults.find(result => 
                result.name.toLowerCase().trim() === contributorKey.toLowerCase().trim()
              );
            }
            
            // If still no match, try partial matching
            if (!scalingResult) {
              scalingResult = scalingResults.find(result => 
                result.name.toLowerCase().includes(contributorKey.toLowerCase()) ||
                contributorKey.toLowerCase().includes(result.name.toLowerCase())
              );
            }
            
            console.log(`ExportDataService: Looking for collaboration score for ${contributorKey}:`, scalingResult);
            const value = scalingResult ? scalingResult.scale : 0;
            console.log(`ExportDataService: Using collaboration score ${value} for ${contributorKey}`);
            row.push(value);
          } else {
            const value = this.getMetricValueFromRepoData(metric, contributorKey, repoData, startDate, endDate);
          row.push(value);
          }
        });
        
        
        rows.push(row);
      });
    } else if (groupBy === 'date') {
      // Generate one row per day (inclusive of both start and end dates)
      const startDay = new Date(startDate);
      startDay.setHours(0, 0, 0, 0);
      const endDay = new Date(endDate);
      endDay.setHours(0, 0, 0, 0);
      const msPerDay = 1000 * 60 * 60 * 24;
      const days = Math.floor((endDay.getTime() - startDay.getTime()) / msPerDay) + 1;

      for (let i = 0; i < Math.max(days, 1); i++) {
        const date = new Date(startDate);
        date.setDate(date.getDate() + i);
        
        const row: (string | number)[] = [];
        
        // Date
        row.push(date.toISOString().split('T')[0]);
        
        // Generate metrics for this date
        metrics.forEach(metric => {
          if (metric === 'collaboration_score') {
            // For date grouping, use average collaboration score
            const avgCollaborationScore = scalingResults.length > 0 
              ? scalingResults.reduce((sum, result) => sum + result.scale, 0) / scalingResults.length 
              : 0;
            row.push(avgCollaborationScore);
          } else {
          const value = this.getMetricValueForDate(metric, date, repoData);
          row.push(value);
          }
        });
        
        
        rows.push(row);
      }
    } else {
      // No grouping - single row with totals
      const row: (string | number)[] = [];
      
      // Generate total metrics
      metrics.forEach(metric => {
        if (metric === 'collaboration_score') {
          // For no grouping, use average collaboration score
          const avgCollaborationScore = scalingResults.length > 0 
            ? scalingResults.reduce((sum, result) => sum + result.scale, 0) / scalingResults.length 
            : 0;
          row.push(avgCollaborationScore);
        } else {
          const value = this.getTotalMetricValueDirect(metric, repoData, startDate, endDate);
        row.push(value);
        }
      });
      
      
      rows.push(row);
    }

    return rows;
  }


  /**
   * Get metric value directly from repository data for a specific contributor
   */
  private getMetricValueFromRepoData(
    metric: string, 
    contributorKey: string, 
    repoData: SerializableRepoData,
    startDate: Date,
    endDate: Date
  ): number | string {
    // Filter commits for this contributor within the date range
    const allCommits = repoData.allCommits || [];
    console.log(`ExportDataService: Calculating ${metric} for ${contributorKey}`);
    console.log(`ExportDataService: Total commits in repo: ${allCommits.length}`);
    console.log(`ExportDataService: Date range: ${startDate.toISOString()} to ${endDate.toISOString()}`);
    
    const commits = allCommits.filter((commit: any) => {
      const commitDate = new Date(commit.value.timestamp);
      return commit.value.contributorName === contributorKey &&
        commitDate >= startDate &&
        commitDate <= endDate;
    });
    
    console.log(`ExportDataService: Commits for ${contributorKey} in date range: ${commits.length}`);
    if (commits.length > 0) {
      console.log(`ExportDataService: Sample commit for ${contributorKey}:`, commits[0]);
    } else {
      // Let's see what commits exist for this contributor
      const contributorCommits = allCommits.filter((commit: any) => 
        commit.value.contributorName === contributorKey
      );
      console.log(`ExportDataService: Total commits for ${contributorKey}: ${contributorCommits.length}`);
      if (contributorCommits.length > 0) {
        console.log(`ExportDataService: Sample commit date: ${contributorCommits[0].value.timestamp}`);
        const sampleCommitDate = new Date(contributorCommits[0].value.timestamp);
        console.log(`ExportDataService: Is within range? ${sampleCommitDate >= startDate && sampleCommitDate <= endDate}`);
      }
    }

    switch (metric) {
      case 'total_commits':
        return commits.length;
        
      case 'commit_frequency':
        // Commits per day
        const days = Math.max(1, Math.ceil((endDate.getTime() - startDate.getTime()) / (1000 * 60 * 60 * 24)));
        return commits.length / days;
        
      case 'commit_size':
        // Average LOC per commit
        if (commits.length === 0) return 0;
        const totalLines = commits.reduce((sum: number, commit: any) => {
          return sum + commit.value.fileData.reduce((fileSum: number, file: any) => 
            fileSum + file.newLines, 0
          );
        }, 0);
        return totalLines / commits.length;
        
      case 'lines_added':
        return commits.reduce((sum: number, commit: any) => {
          return sum + commit.value.fileData.reduce((fileSum: number, file: any) => 
            fileSum + file.newLines, 0
          );
        }, 0);
        
      case 'lines_deleted':
        return commits.reduce((sum: number, commit: any) => {
          return sum + commit.value.fileData.reduce((fileSum: number, file: any) => 
            fileSum + file.deletedLines, 0
          );
        }, 0);
        
      case 'lines_modified':
        return commits.reduce((sum: number, commit: any) => {
          return sum + commit.value.fileData.reduce((fileSum: number, file: any) => 
            fileSum + file.newLines + file.deletedLines, 0
          );
        }, 0);
        
      case 'net_lines':
        return commits.reduce((sum: number, commit: any) => {
          return sum + commit.value.fileData.reduce((fileSum: number, file: any) => 
            fileSum + file.newLines - file.deletedLines, 0
          );
        }, 0);
        
      case 'file_count':
        return commits.reduce((sum: number, commit: any) => {
          return sum + commit.value.fileData.length;
        }, 0);
        
      case 'collaboration_score':
        // Collaboration score is handled by scaling results so just copy from there
        return 0;
        
      case 'active_days':
        // Count unique days with commits
        const uniqueDays = new Set();
        commits.forEach((commit: any) => {
          const day = new Date(commit.value.timestamp).toDateString();
          uniqueDays.add(day);
        });
        return uniqueDays.size;
        
      case 'first_commit':
        // Date of first commit in DD/MM/YYYY format
        if (commits.length === 0) return '';
        const firstCommit = commits.reduce((earliest: any, commit: any) => 
          new Date(commit.value.timestamp) < new Date(earliest.value.timestamp) ? commit : earliest
        );
        const firstDate = new Date(firstCommit.value.timestamp);
        return firstDate.toLocaleDateString('en-GB'); // DD/MM/YYYY format
        
      case 'last_commit':
        // Date of last commit in DD/MM/YYYY format
        if (commits.length === 0) return '';
        const lastCommit = commits.reduce((latest: any, commit: any) => 
          new Date(commit.value.timestamp) > new Date(latest.value.timestamp) ? commit : latest
        );
        const lastDate = new Date(lastCommit.value.timestamp);
        return lastDate.toLocaleDateString('en-GB'); // DD/MM/YYYY format
        
      default:
        return 0;
    }
  }

  /**
   * Get total metric value directly from repository data
   */
  private getTotalMetricValueDirect(
    metric: string, 
    repoData: SerializableRepoData,
    startDate: Date,
    endDate: Date
  ): number | string {
    const commits = (repoData.allCommits || []).filter((commit: any) => 
      commit.value.timestamp >= startDate &&
      commit.value.timestamp <= endDate
    );

    switch (metric) {
      case 'total_commits':
        return commits.length;
        
      case 'commit_frequency':
        // Average commits per day across all contributors
        const days = Math.max(1, Math.ceil((endDate.getTime() - startDate.getTime()) / (1000 * 60 * 60 * 24)));
        return commits.length / days;
        
      case 'commit_size':
        // Average LOC per commit across all contributors
        if (commits.length === 0) return 0;
        const totalLines = commits.reduce((sum: number, commit: any) => {
          return sum + commit.value.fileData.reduce((fileSum: number, file: any) => 
            fileSum + file.newLines, 0
          );
        }, 0);
        return totalLines / commits.length;
        
      case 'lines_added':
        return commits.reduce((sum: number, commit: any) => {
          return sum + commit.value.fileData.reduce((fileSum: number, file: any) => 
            fileSum + file.newLines, 0
          );
        }, 0);
        
      case 'lines_deleted':
        return commits.reduce((sum: number, commit: any) => {
          return sum + commit.value.fileData.reduce((fileSum: number, file: any) => 
            fileSum + file.deletedLines, 0
          );
        }, 0);
        
      case 'lines_modified':
        return commits.reduce((sum: number, commit: any) => {
          return sum + commit.value.fileData.reduce((fileSum: number, file: any) => 
            fileSum + file.newLines + file.deletedLines, 0
          );
        }, 0);
        
      case 'net_lines':
        return commits.reduce((sum: number, commit: any) => {
          return sum + commit.value.fileData.reduce((fileSum: number, file: any) => 
            fileSum + file.newLines - file.deletedLines, 0
          );
        }, 0);
        
      case 'file_count':
        return commits.reduce((sum: number, commit: any) => {
          return sum + commit.value.fileData.length;
        }, 0);
        
      case 'collaboration_score':
        return 0; // This will be calculated at the contributor level
        
      case 'active_days':
        // Count unique days with commits
        const uniqueDays = new Set();
        commits.forEach((commit: any) => {
          const day = new Date(commit.value.timestamp).toDateString();
          uniqueDays.add(day);
        });
        return uniqueDays.size;
        
      case 'first_commit':
        // Date of first commit in DD/MM/YYYY format
        if (commits.length === 0) return '';
        const firstCommit = commits.reduce((earliest: any, commit: any) => 
          new Date(commit.value.timestamp) < new Date(earliest.value.timestamp) ? commit : earliest
        );
        const firstDate = new Date(firstCommit.value.timestamp);
        return firstDate.toLocaleDateString('en-GB'); // DD/MM/YYYY format
        
      case 'last_commit':
        // Date of last commit in DD/MM/YYYY format
        if (commits.length === 0) return '';
        const lastCommit = commits.reduce((latest: any, commit: any) => 
          new Date(commit.value.timestamp) > new Date(latest.value.timestamp) ? commit : latest
        );
        const lastDate = new Date(lastCommit.value.timestamp);
        return lastDate.toLocaleDateString('en-GB'); // DD/MM/YYYY format
        
      default:
        return 0;
    }
  }


  /**
   * Get metric value for a specific date
   */
  private getMetricValueForDate(metric: string, date: Date, repoData: SerializableRepoData): number | string {
    // Filter commits for the specific date
    const startOfDay = new Date(date);
    startOfDay.setHours(0, 0, 0, 0);
    
    const endOfDay = new Date(date);
    endOfDay.setHours(23, 59, 59, 999);
    
    const commitsForDate = (repoData.allCommits || []).filter((commit: any) => 
      commit.value.timestamp >= startOfDay &&
      commit.value.timestamp <= endOfDay
    );

    switch (metric) {
      case 'total_commits':
        return commitsForDate.length;
        
      case 'commit_frequency':
        // Commits per day - for a single day, this is just the count
        return commitsForDate.length;
        
      case 'commit_size':
        // Average LOC per commit for this date
        if (commitsForDate.length === 0) return 0;
        const totalLines = commitsForDate.reduce((sum: number, commit: any) => {
          return sum + commit.value.fileData.reduce((fileSum: number, file: any) => 
            fileSum + file.newLines, 0
          );
        }, 0);
        return totalLines / commitsForDate.length;
        
      case 'lines_added':
        return commitsForDate.reduce((sum: number, commit: any) => {
          return sum + commit.value.fileData.reduce((fileSum: number, file: any) => 
            fileSum + file.newLines, 0
          );
        }, 0);
        
      case 'lines_deleted':
        return commitsForDate.reduce((sum: number, commit: any) => {
          return sum + commit.value.fileData.reduce((fileSum: number, file: any) => 
            fileSum + file.deletedLines, 0
          );
        }, 0);
        
      case 'lines_modified':
        return commitsForDate.reduce((sum: number, commit: any) => {
          return sum + commit.value.fileData.reduce((fileSum: number, file: any) => 
            fileSum + file.newLines + file.deletedLines, 0
          );
        }, 0);
        
      case 'net_lines':
        return commitsForDate.reduce((sum: number, commit: any) => {
          return sum + commit.value.fileData.reduce((fileSum: number, file: any) => 
            fileSum + file.newLines - file.deletedLines, 0
          );
        }, 0);
        
      case 'file_count':
        return commitsForDate.reduce((sum: number, commit: any) => {
          return sum + commit.value.fileData.length;
        }, 0);
        
      case 'collaboration_score':
        // Collaboration score using scaling methodology - measures work distribution among contributors
        // For individual contributor calculation, we need the full repo data
        return 0; // This will be calculated at the contributor level
        
      case 'active_days':
        // For a specific date, this is 1 if there are commits, 0 otherwise
        return commitsForDate.length > 0 ? 1 : 0;
        
      case 'first_commit':
        // Timestamp of first commit on this date
        if (commitsForDate.length === 0) return 0;
        const firstCommit = commitsForDate.reduce((earliest: any, commit: any) => 
          new Date(commit.value.timestamp) < new Date(earliest.value.timestamp) ? commit : earliest
        );
        const firstDate = new Date(firstCommit.value.timestamp);
        return firstDate.toLocaleDateString('en-GB'); // DD/MM/YYYY format
        
      case 'last_commit':
        // Timestamp of last commit on this date
        if (commitsForDate.length === 0) return 0;
        const lastCommit = commitsForDate.reduce((latest: any, commit: any) => 
          commit.value.timestamp > latest.value.timestamp ? commit : latest
        );
        const lastDate = new Date(lastCommit.value.timestamp);
        return lastDate.toLocaleDateString('en-GB'); // DD/MM/YYYY format
        
      default:
        return 0;
    }
  }



  /**
   * Calculate scaling results directly from filtered data
   * This ensures collaboration scores are calculated based on the selected date range
   */
  private calculateScalingResultsFromFilteredData(filteredData: FilteredData): any[] {
    const { repositoryData } = filteredData;
    const contributors = this.getContributorsFromRepoData(repositoryData);
    
    if (contributors.length === 0) {
      return [];
    }

    // Calculate metrics for each contributor based on the filtered data
    const contributorMetrics = contributors.map(contributorKey => {
      const contributorData = repositoryData.contributors.find(c => c.key === contributorKey);
      const contributorName = contributorData ? contributorData.value.name : contributorKey;
      
      // Get commits for this contributor within the filtered date range
      const commits = (repositoryData.allCommits || []).filter((commit: any) => 
        commit.value.contributorName === contributorKey
      );

      // Calculate metrics for this contributor
      const totalCommits = commits.length;
      const totalLines = commits.reduce((sum: number, commit: any) => {
        return sum + commit.value.fileData.reduce((fileSum: number, file: any) => 
          fileSum + file.newLines, 0
        );
      }, 0);
      const locPerCommit = totalCommits > 0 ? totalLines / totalCommits : 0;
      
      // Calculate commits per day based on the date range
      const startDate = filteredData.dateRange?.start || new Date();
      const endDate = filteredData.dateRange?.end || new Date();
      const days = Math.max(1, Math.ceil((endDate.getTime() - startDate.getTime()) / (1000 * 60 * 60 * 24)));
      const commitsPerDay = totalCommits / days;

    return {
        name: contributorKey,
        displayName: contributorName,
        metrics: {
          'Total No. Commits': totalCommits,
          'LOC': totalLines,
          'LOC Per Commit': locPerCommit,
          'Commits Per Day': commitsPerDay
        }
      };
    });

    // Apply scaling logic (simplified version of the server-side scaling)
    const selectedMetrics = ['Total No. Commits', 'LOC', 'LOC Per Commit', 'Commits Per Day'] as const;

    // Normalize metrics for each contributor
    const normalizedScores = contributorMetrics.map(contributor => {
      // Simple percentile ranking within the group
      const percentileScores = selectedMetrics.map((metric) => {
        const allValues = contributorMetrics
          .map(c => c.metrics[metric])
          .filter(v => Number.isFinite(v) && v !== null)
          .sort((a, b) => a - b);
        
        const value = contributor.metrics[metric];
        if (!Number.isFinite(value) || value === null) return 0.5;
        
        const index = allValues.indexOf(value);
        return index / Math.max(allValues.length - 1, 1);
      });

      // Average the percentile scores
      const finalScore = percentileScores.reduce((sum, score) => sum + score, 0) / selectedMetrics.length;

    return {
        name: contributor.name,
        displayName: contributor.displayName,
        scale: Math.round(finalScore * 100) / 100
      };
    });

    return normalizedScores;
  }

  
}

// Export singleton instance
export const exportDataService = ExportDataService.getInstance();
