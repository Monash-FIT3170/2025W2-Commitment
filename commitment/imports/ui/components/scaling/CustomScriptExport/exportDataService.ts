import { DataSelectionConfig } from './DataSelectionPanel';
import { ExportData } from './ExportPreview';
import { meteorCallAsync } from '../../../../api/meteor_interface';
import { 
  FilteredData, 
  SerializableRepoData, 
  AllMetricsData
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
      console.log('ExportDataService: Fetching data for repoUrl:', repoUrl);
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

      // Get all metrics for the filtered data
      const allMetrics: AllMetricsData = await meteorCallAsync<AllMetricsData>("repo.getAllMetrics")(repoUrl);
      
      console.log('ExportDataService: All metrics:', allMetrics);
      console.log('ExportDataService: All metrics keys:', Object.keys(allMetrics || {}));

      // Generate export data from the real repository data
      const exportData = this.generateExportDataFromRepo(config, filteredData, allMetrics);
      
      console.log('ExportDataService: Generated export data:', exportData);
      return exportData;
    } catch (error) {
      console.error('Error fetching export data:', error);
      throw new Error(`Failed to fetch repository data: ${error instanceof Error ? error.message : 'Unknown error'}`);
    }
  }

  /**
   * Generate export data from real repository data
   */
  private generateExportDataFromRepo(
    config: DataSelectionConfig, 
    filteredData: FilteredData, 
    allMetrics: AllMetricsData
  ): ExportData {
    const { branch, dateRange, selectedMetrics, groupBy, includeRawData } = config;
    const startDate = dateRange?.from || new Date();
    const endDate = dateRange?.to || new Date();
    const { repositoryData } = filteredData;
    
    // Get contributors from the repository data
    const contributors = this.getContributorsFromRepoData(repositoryData);
    console.log('ExportDataService: Extracted contributors for export:', contributors);
    
    // Generate headers based on selected metrics and grouping
    const headers = this.generateHeaders(selectedMetrics, groupBy, includeRawData);
    console.log('ExportDataService: Generated headers:', headers);
    
    // Generate rows based on grouping
    const rows = this.generateRowsFromRepoData(
      selectedMetrics, 
      groupBy, 
      includeRawData, 
      contributors, 
      repositoryData,
      allMetrics,
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

  private generateRowsFromRepoData(
    metrics: string[], 
    groupBy: string, 
    includeRawData: boolean, 
    contributors: string[], 
    repoData: SerializableRepoData,
    allMetrics: AllMetricsData,
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
        
        // Get metrics for this contributor from allMetrics
        metrics.forEach(metric => {
          const value = this.getMetricValueFromData(metric, contributor, allMetrics, repoData);
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
        const value = this.getTotalMetricValue(metric, allMetrics, repoData);
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
   * Get metric value from allMetrics data for a specific contributor
   */
  private getMetricValueFromData(
    metric: string, 
    contributor: string, 
    allMetrics: AllMetricsData, 
    repoData: SerializableRepoData
  ): number {
    const contributorMetrics = allMetrics[contributor];
    if (!contributorMetrics) return 0;

    switch (metric) {
      case 'total_commits':
        return contributorMetrics["Total No. Commits"] || 0;
      case 'commit_frequency':
        return contributorMetrics["Commits Per Day"] || 0;
      case 'commit_size':
        return contributorMetrics["LOC Per Commit"] || 0;
      case 'lines_added':
        return contributorMetrics.LOC || 0;
      case 'lines_deleted':
        return 0; // Not directly available in current metrics
      case 'lines_modified':
        return contributorMetrics.LOC || 0;
      case 'net_lines':
        return contributorMetrics.LOC || 0;
      case 'file_count':
        return 0; // Not directly available in current metrics
      case 'contributors':
        return Object.keys(allMetrics).length;
      case 'collaboration_score':
        return 0; // Would need to be calculated
      case 'active_days':
        return this.getActiveDaysForContributor(contributor, repoData);
      case 'first_commit':
        return this.getFirstCommitDateForContributor(contributor, repoData);
      case 'last_commit':
        return this.getLastCommitDateForContributor(contributor, repoData);
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
    
    const commitsForDate = repoData.allCommits.filter((commit: any) => 
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
          commit.value.timestamp < earliest.value.timestamp ? commit : earliest
        );
        return firstCommit.value.timestamp.getTime();
        
      case 'last_commit':
        // Timestamp of last commit on this date
        if (commitsForDate.length === 0) return 0;
        const lastCommit = commitsForDate.reduce((latest: any, commit: any) => 
          commit.value.timestamp > latest.value.timestamp ? commit : latest
        );
        return lastCommit.value.timestamp.getTime();
        
      default:
        return 0;
    }
  }

  /**
   * Get total metric value across all contributors
   */
  private getTotalMetricValue(metric: string, allMetrics: AllMetricsData, repoData: SerializableRepoData): number {
    const contributors = Object.keys(allMetrics);
    
    switch (metric) {
      case 'total_commits':
        return contributors.reduce((sum, contributor) => 
          sum + (allMetrics[contributor]["Total No. Commits"] || 0), 0);
      case 'commit_frequency':
        return contributors.reduce((sum, contributor) => 
          sum + (allMetrics[contributor]["Commits Per Day"] || 0), 0) / contributors.length;
      case 'commit_size':
        return contributors.reduce((sum, contributor) => 
          sum + (allMetrics[contributor]["LOC Per Commit"] || 0), 0) / contributors.length;
      case 'lines_added':
        return contributors.reduce((sum, contributor) => 
          sum + (allMetrics[contributor].LOC || 0), 0);
      case 'lines_deleted':
        return 0; // Not directly available
      case 'lines_modified':
        return contributors.reduce((sum, contributor) => 
          sum + (allMetrics[contributor].LOC || 0), 0);
      case 'net_lines':
        return contributors.reduce((sum, contributor) => 
          sum + (allMetrics[contributor].LOC || 0), 0);
      case 'file_count':
        return 0; // Not directly available
      case 'contributors':
        return contributors.length;
      case 'collaboration_score':
        return 0; // Would need to be calculated
      case 'active_days':
        return this.getTotalActiveDays(repoData);
      case 'first_commit':
        return this.getFirstCommitDate(repoData);
      case 'last_commit':
        return this.getLastCommitDate(repoData);
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
    const commits = repoData.allCommits.filter((commit: any) => 
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

    const commits = repoData.allCommits.filter((commit: any) => 
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
    const commits = repoData.allCommits.filter((commit: any) => 
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
   * Get active days for a specific contributor
   */
  private getActiveDaysForContributor(contributor: string, repoData: SerializableRepoData): number {
    const commits = repoData.allCommits.filter((commit: any) => 
      commit.value.contributorName === contributor
    );

    const uniqueDays = new Set();
    commits.forEach((commit: any) => {
      const day = commit.value.timestamp.toDateString();
      uniqueDays.add(day);
    });

    return uniqueDays.size;
  }

  /**
   * Get first commit date for a specific contributor
   */
  private getFirstCommitDateForContributor(contributor: string, repoData: SerializableRepoData): number {
    const commits = repoData.allCommits.filter((commit: any) => 
      commit.value.contributorName === contributor
    );

    if (commits.length === 0) return 0;

    const firstCommit = commits.reduce((earliest: any, commit: any) => 
      commit.value.timestamp < earliest.value.timestamp ? commit : earliest
    );

    return firstCommit.value.timestamp.getTime();
  }

  /**
   * Get last commit date for a specific contributor
   */
  private getLastCommitDateForContributor(contributor: string, repoData: SerializableRepoData): number {
    const commits = repoData.allCommits.filter((commit: any) => 
      commit.value.contributorName === contributor
    );

    if (commits.length === 0) return 0;

    const lastCommit = commits.reduce((latest: any, commit: any) => 
      commit.value.timestamp > latest.value.timestamp ? commit : latest
    );

    return lastCommit.value.timestamp.getTime();
  }

  /**
   * Get total active days across all contributors
   */
  private getTotalActiveDays(repoData: SerializableRepoData): number {
    const uniqueDays = new Set();
    repoData.allCommits.forEach((commit: any) => {
      const day = commit.value.timestamp.toDateString();
      uniqueDays.add(day);
    });

    return uniqueDays.size;
  }

  /**
   * Get first commit date across all contributors
   */
  private getFirstCommitDate(repoData: SerializableRepoData): number {
    if (repoData.allCommits.length === 0) return 0;

    const firstCommit = repoData.allCommits.reduce((earliest: any, commit: any) => 
      commit.value.timestamp < earliest.value.timestamp ? commit : earliest
    );

    return firstCommit.value.timestamp.getTime();
  }

  /**
   * Get last commit date across all contributors
   */
  private getLastCommitDate(repoData: SerializableRepoData): number {
    if (repoData.allCommits.length === 0) return 0;

    const lastCommit = repoData.allCommits.reduce((latest: any, commit: any) => 
      commit.value.timestamp > latest.value.timestamp ? commit : latest
    );

    return lastCommit.value.timestamp.getTime();
  }
}

// Export singleton instance
export const exportDataService = ExportDataService.getInstance();
