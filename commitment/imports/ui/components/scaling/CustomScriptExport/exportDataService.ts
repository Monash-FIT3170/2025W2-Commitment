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

      // Generate export data from the real repository data
      const exportData = this.generateExportDataFromRepoDirect(config, filteredData);
      
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
    filteredData: FilteredData
  ): ExportData {
    const { branch, dateRange, selectedMetrics, groupBy, includeRawData } = config;
    const startDate = dateRange?.from || new Date();
    const endDate = dateRange?.to || new Date();
    const { repositoryData } = filteredData;
    
    // Get contributors from the repository data
    const contributors = this.getContributorsFromRepoData(repositoryData);
    console.log('ExportDataService: Extracted contributors for direct export:', contributors);
    
    // Generate headers based on selected metrics and grouping
    const headers = this.generateHeaders(selectedMetrics, groupBy, includeRawData);
    console.log('ExportDataService: Generated headers:', headers);
    
    // Generate rows based on grouping
    const rows = this.generateRowsFromRepoDataDirect(
      selectedMetrics, 
      groupBy, 
      includeRawData, 
      contributors, 
      repositoryData,
      startDate,
      endDate
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
    
    const contributors = repoData.contributors.map((contributor: any) => contributor.value.name);
    console.log('ExportDataService: Extracted contributors:', contributors);
    return contributors;
  }

  private generateHeaders(metrics: string[], groupBy: string, includeRawData: boolean): string[] {
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

    // Add raw data columns if requested
    if (includeRawData) {
      headers.push('commit_count', 'files_changed', 'lines_added', 'lines_deleted');
    }

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
      'contributors': 'contributor_count',
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
    includeRawData: boolean, 
    contributors: string[], 
    repoData: SerializableRepoData,
    startDate: Date,
    endDate: Date
  ): (string | number)[][] {
    const rows: (string | number)[][] = [];

    if (groupBy === 'contributor') {
      // Generate one row per contributor
      contributors.forEach(contributor => {
        const row: (string | number)[] = [];
        
        // Contributor info
        row.push(contributor);
        row.push(contributor); // Use name as both email and name for now
        
        // Get metrics for this contributor directly from repository data
        metrics.forEach(metric => {
          const value = this.getMetricValueFromRepoData(metric, contributor, repoData, startDate, endDate);
          row.push(value);
        });
        
        // Raw data if requested
        if (includeRawData) {
          const rawData = this.getRawDataForContributor(contributor, repoData, startDate, endDate);
          row.push(rawData.commitCount);
          row.push(rawData.filesChanged);
          row.push(rawData.linesAdded);
          row.push(rawData.linesDeleted);
        }
        
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
          const value = this.getMetricValueForDate(metric, date, repoData);
          row.push(value);
        });
        
        // Raw data if requested
        if (includeRawData) {
          const rawData = this.getRawDataForDate(date, repoData);
          row.push(rawData.commitCount);
          row.push(rawData.filesChanged);
          row.push(rawData.linesAdded);
          row.push(rawData.linesDeleted);
        }
        
        rows.push(row);
      }
    } else {
      // No grouping - single row with totals
      const row: (string | number)[] = [];
      
      // Generate total metrics
      metrics.forEach(metric => {
        const value = this.getTotalMetricValueDirect(metric, repoData, startDate, endDate);
        row.push(value);
      });
      
      // Raw data if requested
      if (includeRawData) {
        const rawData = this.getTotalRawData(repoData, startDate, endDate);
        row.push(rawData.commitCount);
        row.push(rawData.filesChanged);
        row.push(rawData.linesAdded);
        row.push(rawData.linesDeleted);
      }
      
      rows.push(row);
    }

    return rows;
  }


  /**
   * Get metric value directly from repository data for a specific contributor
   */
  private getMetricValueFromRepoData(
    metric: string, 
    contributor: string, 
    repoData: SerializableRepoData,
    startDate: Date,
    endDate: Date
  ): number {
    // Filter commits for this contributor within the date range
    const allCommits = repoData.allCommits || [];
    console.log(`ExportDataService: Calculating ${metric} for ${contributor}`);
    console.log(`ExportDataService: Total commits in repo: ${allCommits.length}`);
    console.log(`ExportDataService: Date range: ${startDate.toISOString()} to ${endDate.toISOString()}`);
    
    const commits = allCommits.filter((commit: any) => {
      const commitDate = new Date(commit.value.timestamp);
      return commit.value.contributorName === contributor &&
        commitDate >= startDate &&
        commitDate <= endDate;
    });
    
    console.log(`ExportDataService: Commits for ${contributor} in date range: ${commits.length}`);
    if (commits.length > 0) {
      console.log(`ExportDataService: Sample commit for ${contributor}:`, commits[0]);
    } else {
      // Let's see what commits exist for this contributor
      const contributorCommits = allCommits.filter((commit: any) => 
        commit.value.contributorName === contributor
      );
      console.log(`ExportDataService: Total commits for ${contributor}: ${contributorCommits.length}`);
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
        
      case 'contributors':
        // Count unique contributors in the date range
        const uniqueContributors = new Set();
        (repoData.allCommits || []).forEach((commit: any) => {
          if (commit.value.timestamp >= startDate && commit.value.timestamp <= endDate) {
            uniqueContributors.add(commit.value.contributorName);
          }
        });
        return uniqueContributors.size;
        
      case 'collaboration_score':
        // Simple collaboration score based on unique contributors vs total commits
        const uniqueContributorsCount = new Set();
        (repoData.allCommits || []).forEach((commit: any) => {
          if (commit.value.timestamp >= startDate && commit.value.timestamp <= endDate) {
            uniqueContributorsCount.add(commit.value.contributorName);
          }
        });
        const totalCommits = (repoData.allCommits || []).filter((commit: any) => 
          commit.value.timestamp >= startDate && commit.value.timestamp <= endDate
        ).length;
        return totalCommits > 0 ? uniqueContributorsCount.size / totalCommits : 0;
        
      case 'active_days':
        // Count unique days with commits
        const uniqueDays = new Set();
        commits.forEach((commit: any) => {
          const day = new Date(commit.value.timestamp).toDateString();
          uniqueDays.add(day);
        });
        return uniqueDays.size;
        
      case 'first_commit':
        // Timestamp of first commit
        if (commits.length === 0) return 0;
        const firstCommit = commits.reduce((earliest: any, commit: any) => 
          new Date(commit.value.timestamp) < new Date(earliest.value.timestamp) ? commit : earliest
        );
        return new Date(firstCommit.value.timestamp).getTime();
        
      case 'last_commit':
        // Timestamp of last commit
        if (commits.length === 0) return 0;
        const lastCommit = commits.reduce((latest: any, commit: any) => 
          commit.value.timestamp > latest.value.timestamp ? commit : latest
        );
        return new Date(lastCommit.value.timestamp).getTime();
        
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
  ): number {
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
        
      case 'contributors':
        // Count unique contributors in the date range
        const uniqueContributors = new Set();
        commits.forEach((commit: any) => {
          uniqueContributors.add(commit.value.contributorName);
        });
        return uniqueContributors.size;
        
      case 'collaboration_score':
        // Simple collaboration score based on unique contributors vs total commits
        const uniqueContributorsCount = new Set();
        commits.forEach((commit: any) => {
          uniqueContributorsCount.add(commit.value.contributorName);
        });
        return commits.length > 0 ? uniqueContributorsCount.size / commits.length : 0;
        
      case 'active_days':
        // Count unique days with commits
        const uniqueDays = new Set();
        commits.forEach((commit: any) => {
          const day = new Date(commit.value.timestamp).toDateString();
          uniqueDays.add(day);
        });
        return uniqueDays.size;
        
      case 'first_commit':
        // Timestamp of first commit
        if (commits.length === 0) return 0;
        const firstCommit = commits.reduce((earliest: any, commit: any) => 
          new Date(commit.value.timestamp) < new Date(earliest.value.timestamp) ? commit : earliest
        );
        return new Date(firstCommit.value.timestamp).getTime();
        
      case 'last_commit':
        // Timestamp of last commit
        if (commits.length === 0) return 0;
        const lastCommit = commits.reduce((latest: any, commit: any) => 
          commit.value.timestamp > latest.value.timestamp ? commit : latest
        );
        return new Date(lastCommit.value.timestamp).getTime();
        
      default:
        return 0;
    }
  }


  /**
   * Get metric value for a specific date
   */
  private getMetricValueForDate(metric: string, date: Date, repoData: SerializableRepoData): number {
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
        
      case 'contributors':
        // Count unique contributors for this date
        const uniqueContributors = new Set();
        commitsForDate.forEach((commit: any) => {
          uniqueContributors.add(commit.value.contributorName);
        });
        return uniqueContributors.size;
        
      case 'collaboration_score':
        // Simple collaboration score based on unique contributors vs total commits
        const uniqueContributorsCount = new Set();
        commitsForDate.forEach((commit: any) => {
          uniqueContributorsCount.add(commit.value.contributorName);
        });
        return commitsForDate.length > 0 ? uniqueContributorsCount.size / commitsForDate.length : 0;
        
      case 'active_days':
        // For a specific date, this is 1 if there are commits, 0 otherwise
        return commitsForDate.length > 0 ? 1 : 0;
        
      case 'first_commit':
        // Timestamp of first commit on this date
        if (commitsForDate.length === 0) return 0;
        const firstCommit = commitsForDate.reduce((earliest: any, commit: any) => 
          new Date(commit.value.timestamp) < new Date(earliest.value.timestamp) ? commit : earliest
        );
        return new Date(firstCommit.value.timestamp).getTime();
        
      case 'last_commit':
        // Timestamp of last commit on this date
        if (commitsForDate.length === 0) return 0;
        const lastCommit = commitsForDate.reduce((latest: any, commit: any) => 
          commit.value.timestamp > latest.value.timestamp ? commit : latest
        );
        return new Date(lastCommit.value.timestamp).getTime();
        
      default:
        return 0;
    }
  }


  /**
   * Get raw data for a specific contributor
   */
  private getRawDataForContributor(
    contributor: string, 
    repoData: SerializableRepoData, 
    startDate: Date, 
    endDate: Date
  ): { commitCount: number; filesChanged: number; linesAdded: number; linesDeleted: number } {
    const commits = (repoData.allCommits || []).filter((commit: any) => 
      commit.value.contributorName === contributor &&
      commit.value.timestamp >= startDate &&
      commit.value.timestamp <= endDate
    );

    let filesChanged = 0;
    let linesAdded = 0;
    let linesDeleted = 0;

    commits.forEach((commit: any) => {
      filesChanged += commit.value.fileData.length;
      commit.value.fileData.forEach((file: any) => {
        linesAdded += file.newLines;
        linesDeleted += file.deletedLines;
      });
    });

    return {
      commitCount: commits.length,
      filesChanged,
      linesAdded,
      linesDeleted
    };
  }

  /**
   * Get raw data for a specific date
   */
  private getRawDataForDate(date: Date, repoData: SerializableRepoData): { commitCount: number; filesChanged: number; linesAdded: number; linesDeleted: number } {
    const startOfDay = new Date(date);
    startOfDay.setHours(0, 0, 0, 0);
    const endOfDay = new Date(date);
    endOfDay.setHours(23, 59, 59, 999);

    const commits = (repoData.allCommits || []).filter((commit: any) => 
      commit.value.timestamp >= startOfDay &&
      commit.value.timestamp <= endOfDay
    );

    let filesChanged = 0;
    let linesAdded = 0;
    let linesDeleted = 0;

    commits.forEach((commit: any) => {
      filesChanged += commit.value.fileData.length;
      commit.value.fileData.forEach((file: any) => {
        linesAdded += file.newLines;
        linesDeleted += file.deletedLines;
      });
    });

    return {
      commitCount: commits.length,
      filesChanged,
      linesAdded,
      linesDeleted
    };
  }

  /**
   * Get total raw data across all contributors
   */
  private getTotalRawData(repoData: SerializableRepoData, startDate: Date, endDate: Date): { commitCount: number; filesChanged: number; linesAdded: number; linesDeleted: number } {
    const commits = (repoData.allCommits || []).filter((commit: any) => 
      commit.value.timestamp >= startDate &&
      commit.value.timestamp <= endDate
    );

    let filesChanged = 0;
    let linesAdded = 0;
    let linesDeleted = 0;

    commits.forEach((commit: any) => {
      filesChanged += commit.value.fileData.length;
      commit.value.fileData.forEach((file: any) => {
        linesAdded += file.newLines;
        linesDeleted += file.deletedLines;
      });
    });

    return {
      commitCount: commits.length,
      filesChanged,
      linesAdded,
      linesDeleted
    };
  }

}

// Export singleton instance
export const exportDataService = ExportDataService.getInstance();
