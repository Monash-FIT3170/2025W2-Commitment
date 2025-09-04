import Papa from 'papaparse';
import type { UserScalingSummary } from "../../../api/types";
import type { GradingSheetRow, ParseResult } from "../utils/GradingSheetParser";

/**
 * Calculate final grades by matching contributors with grading sheet data.
 * This function runs on the client side for UI operations.
 */
export function calculateFinalGrades(
    scalingResults: UserScalingSummary[], 
    gradingData: GradingSheetRow[]
): UserScalingSummary[] {
    return scalingResults.map(contributor => {
        const matchingStudent = gradingData.find(student => {
        const studentName = student.fullName.toLowerCase().trim();
        const contributorName = contributor.name.toLowerCase().trim();
        const studentEmail = student.emailAddress.toLowerCase().trim();
        if (studentName === contributorName) {
            return true;
        }
        return contributor.aliases.some(alias => 
            alias.email.toLowerCase().trim() === studentEmail
        );
        });

        if (matchingStudent) {
        const percentageGrade = (matchingStudent.grade / matchingStudent.maximumGrade) * 100;
        const finalGrade = percentageGrade * contributor.scale;        
        return {
            ...contributor,
            finalGrade: Math.round(finalGrade * 100) / 100
        };
        }
        
        return {
        ...contributor,
        finalGrade: null
        };
    });
}

/**
 * Generate updated grading sheet CSV with scaled grades.
 * This function runs on the client side for file download operations.
 */
export function generateScaledGradingSheet(
    originalParseResult: ParseResult,
    scalingResults: UserScalingSummary[]
    ): File {
  // Required headers for CSV output
    const REQUIRED_HEADERS = [
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

    try {
        let dataToExport = originalParseResult.data || [];
        
        // Map UserScalingSummary to grade mapping
        if (scalingResults && dataToExport.length > 0) {
        const finalGradeMap = new Map<string, number>();
        const emailToGradeMap = new Map<string, number>();

        scalingResults.forEach(result => {
            if (result.finalGrade !== null) {
            finalGradeMap.set(result.name.toLowerCase().trim(), result.finalGrade);
            result.aliases.forEach(alias => {
                emailToGradeMap.set(alias.email.toLowerCase().trim(), result.finalGrade!);
            });
            }
        });
        
        // Update grades in the exported data
        dataToExport = dataToExport.map(student => {
            const studentName = student.fullName.toLowerCase().trim();
            const studentEmail = student.emailAddress.toLowerCase().trim();
            const finalGrade = finalGradeMap.get(studentName) || emailToGradeMap.get(studentEmail);
            if (finalGrade !== undefined) {
            return {
                ...student,
                grade: finalGrade
            };
            }
            return student;
        });
    }
    
    // Convert to CSV format
    const csvData = dataToExport.map(row => {
        const csvRow: Record<string, unknown> = {};
        const rowPropertyMapping = [
            row.identifier,
            row.fullName,
            row.idNumber,
            row.emailAddress,
            row.status,
            row.grade,
            row.maximumGrade,
            row.gradeCanBeChanged ? 'Yes' : 'No',
            row.lastModifiedSubmission || '-',
            row.lastModifiedGrade || '-',
            row.feedbackComments
        ];
        REQUIRED_HEADERS.forEach((header, index) => {
            csvRow[header] = rowPropertyMapping[index];
        });
        
        return csvRow;
    });
    
    // Generate CSV content and create file
    const csvContent = Papa.unparse(csvData);
    const blob = new Blob([csvContent], { type: 'text/csv' });
    const filename = 'scaled_grading_sheet.csv';
    return new File([blob], filename, { type: 'text/csv' });
    
    } catch (error) {
    throw new Error(`Failed to generate scaled grading sheet: ${error instanceof Error ? error.message : 'Unknown error'}`);
    }
}
