import Papa from 'papaparse';

export interface GradingSheetRow {
    identifier: string;
    fullName: string;
    idNumber: string;
    emailAddress: string;
    status: string;
    grade: number;
    maximumGrade: number;
    gradeCanBeChanged: boolean;
    lastModifiedSubmission: string | null;
    lastModifiedGrade: string | null;
    feedbackComments: string;
}

export interface ParseResult {
    success: boolean;
    data?: GradingSheetRow[];
    error?: string;
    rowCount?: number;
    warnings?: string[];
}

export interface ValidationResult {
    valid: boolean;
    errors: string[];
    warnings: string[];
}

export class GradingSheetParserService {
  // Required column headers (as they appear in the CSV)
    private static readonly REQUIRED_HEADERS = [
        'Identifier',
        'Full name', 
        'ID number',
        'Email address',
        'Status',
        'Grade',
        'Maximum grade',
        'Grade can be changed',
        'Last modified (submission)',
        'Last modified (grade)',
        'Feedback comments'
    ];

    /**
     * Parse a grading sheet CSV file into structured data
     */
    static async parseGradingSheet(file: File): Promise<ParseResult> {
        return new Promise((resolve) => {
        Papa.parse(file, {
            header: true,
            skipEmptyLines: true,
            transformHeader: (header: string) => header.trim(),
            complete: (results) => {
            if (results.errors.length > 0) {
                resolve({
                success: false,
                error: `CSV parsing error: ${results.errors[0].message}`
                });
                return;
            }

            try {
                // Validate the Grading Sheet's structure??
                const validation = this.validateSheetStructure(
                results.data as Record<string, unknown>[], 
                Object.keys(results.data[0] || {})
                );
                
                if (!validation.valid) {
                resolve({
                    success: false,
                    error: validation.errors.join('; ')
                });
                return;
                }

                // Transform raw CSV data to typed objects
                const transformedData = this.transformRawData(results.data as Record<string, unknown>[]);
                
                resolve({
                success: true,
                data: transformedData,
                rowCount: transformedData.length,
                warnings: validation.warnings
                });
            } catch (error) {
                resolve({
                success: false,
                error: `Data transformation error: ${error instanceof Error ? error.message : 'Unknown error'}`
                });
            }
            },
            error: (error) => {
            resolve({
                success: false,
                error: `Failed to parse CSV: ${error.message}`
            });
            }
        });
        });
    }

    /**
     * Validate that the CSV has the correct structure and required columns
     */
    private static validateSheetStructure(data: Record<string, unknown>[], headers: string[]): ValidationResult {
        const errors: string[] = [];
        const warnings: string[] = [];

        // Check if file is empty
        if (!data || data.length === 0) {
        errors.push("CSV file contains no data rows");
        return { valid: false, errors, warnings };
        }

        // Check for required headers
        const missingHeaders = this.REQUIRED_HEADERS.filter(
        required => !headers.some(header => header.toLowerCase() === required.toLowerCase())
        );

        if (missingHeaders.length > 0) {
        errors.push(`Missing required columns: ${missingHeaders.join(', ')}`);
        }

        // Check for extra headers (warn but don't fail)
        const extraHeaders = headers.filter(
        header => !this.REQUIRED_HEADERS.some(required => 
            required.toLowerCase() === header.toLowerCase()
        )
        );

        if (extraHeaders.length > 0) {
        warnings.push(`Extra columns found (will be ignored): ${extraHeaders.join(', ')}`);
        }

        // Validate data integrity for first few rows
        const sampleSize = Math.min(3, data.length);
        for (let i = 0; i < sampleSize; i++) {
        const row = data[i];
        const rowErrors = this.validateRowData(row, i + 1);
        errors.push(...rowErrors);
        }

        return {
        valid: errors.length === 0,
        errors,
        warnings
        };
    }

    /**
     * Validate individual row data
     */
    private static validateRowData(row: Record<string, unknown>, rowNumber: number): string[] {
        const errors: string[] = [];

        // Check for completely empty rows
        const hasAnyData = Object.values(row).some(value => {
        if (value === null || value === undefined) return false;
        const stringValue = typeof value === 'string' ? value : String(value);
        return stringValue.trim() !== '';
        });

        if (!hasAnyData) {
        errors.push(`Row ${rowNumber}: Completely empty row`);
        return errors;
        }

        // Validate grade is a number
        const grade = row.Grade;
        if (grade !== null && grade !== undefined && grade !== '' && Number.isNaN(Number(grade))) {
        const gradeStr = typeof grade === 'string' ? grade : String(grade);
        errors.push(`Row ${rowNumber}: Grade "${gradeStr}" is not a valid number`);
        }

        // Validate maximum grade is a number
        const maxGrade = row['Maximum grade'];
        if (maxGrade !== null && maxGrade !== undefined && maxGrade !== '' && Number.isNaN(Number(maxGrade))) {
        const maxGradeStr = typeof maxGrade === 'string' ? maxGrade : String(maxGrade);
        errors.push(`Row ${rowNumber}: Maximum grade "${maxGradeStr}" is not a valid number`);
        }

        // Validate email format (basic check)
        const email = row['Email address'];
        if (email && typeof email === 'string' && email.trim() !== '' && !email.includes('@')) {
        errors.push(`Row ${rowNumber}: Invalid email format "${email}"`);
        }

        return errors;
    }

    /**
     * Transform raw CSV data into typed GradingSheetRow objects
     */
    private static transformRawData(rawData: Record<string, unknown>[]): GradingSheetRow[] {
        return rawData.map(row => ({
        identifier: this.safeStringConvert(row.Identifier),
        fullName: this.safeStringConvert(row['Full name']),
        idNumber: this.safeStringConvert(row['ID number']),
        emailAddress: this.safeStringConvert(row['Email address']),
        status: this.safeStringConvert(row.Status),
        grade: this.parseNumericValue(row.Grade),
        maximumGrade: this.parseNumericValue(row['Maximum grade']),
        gradeCanBeChanged: this.parseBooleanValue(row['Grade can be changed']),
        lastModifiedSubmission: this.parseOptionalString(row['Last modified (submission)']),
        lastModifiedGrade: this.parseOptionalString(row['Last modified (grade)']),
        feedbackComments: this.safeStringConvert(row['Feedback comments'])
        }));
    }

    /**
     * Safely convert unknown value to string
     */
    private static safeStringConvert(value: unknown): string {
        if (value === null || value === undefined) {
        return '';
        }
        return typeof value === 'string' ? value.trim() : String(value).trim();
    }

    /**
     * Helper to parse numeric values safely
     */
    private static parseNumericValue(value: unknown): number {
        if (value === null || value === undefined || value === '') {
        return 0;
        }
        const parsed = Number(value);
        return Number.isNaN(parsed) ? 0 : parsed;
    }

    /**
     * Helper to parse boolean values from CSV text
     */
    private static parseBooleanValue(value: unknown): boolean {
        if (value === null || value === undefined) {
        return false;
        }
        const stringValue = this.safeStringConvert(value).toLowerCase();
        return stringValue === 'yes' || stringValue === 'true' || stringValue === '1';
    }

    /**
     * Helper to parse optional string values (handle dashes and empty strings)
     */
    private static parseOptionalString(value: unknown): string | null {
        if (value === null || value === undefined) {
        return null;
        }
        const stringValue = this.safeStringConvert(value);
        return stringValue === '-' || stringValue === '' ? null : stringValue;
    }

    /**
     * Get summary statistics from parsed data
     */
    static getDataSummary(data: GradingSheetRow[]): {
        totalStudents: number;
        averageGrade: number;
        gradeRange: { min: number; max: number };
        submissionStatuses: Record<string, number>;
    } {
        const totalStudents = data.length;
        const grades = data.map(row => row.grade).filter(grade => grade > 0);
        
        const averageGrade = grades.length > 0 
        ? grades.reduce((sum, grade) => sum + grade, 0) / grades.length 
        : 0;

        const gradeRange = grades.length > 0 
        ? { min: Math.min(...grades), max: Math.max(...grades) }
        : { min: 0, max: 0 };

        const submissionStatuses = data.reduce((acc, row) => {
        const status = row.status || 'Unknown';
        acc[status] = (acc[status] || 0) + 1;
        return acc;
        }, {} as Record<string, number>);

        return {
        totalStudents,
        averageGrade: Math.round(averageGrade * 100) / 100,
        gradeRange,
        submissionStatuses
        };
    }
}
