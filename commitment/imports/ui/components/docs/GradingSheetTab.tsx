import React from "react";
import { FileSpreadsheet, Upload } from "lucide-react";

const GradingSheetTab: React.FC = () => {
  return (
    <div className="space-y-6">
      <div className="p-6 bg-git-bg-elevated border border-git-stroke-primary rounded-lg">
        <div className="flex items-center gap-2 mb-4">
          <FileSpreadsheet className="h-6 w-6 text-git-accent-primary" />
          <h2 className="text-2xl font-semibold text-git-text-primary">5.0 Grading Sheet Upload</h2>
        </div>
        <p className="text-git-text-secondary mb-6">
          Upload and manage grading sheets to map student contributions to grades and assessments.
        </p>

        <div className="space-y-4 text-git-text-secondary">
          <div>
            <h3 className="text-lg font-semibold text-git-text-primary mb-2 flex items-center gap-2">
              <Upload className="h-5 w-5 text-git-accent-primary" />
              What is a Grading Sheet?
            </h3>
            <p className="ml-6">
              A grading sheet is a CSV or Excel file that maps contributor information to assessment criteria.
              It allows educators to link repository contributions to student grades and track assessment outcomes.
            </p>
          </div>

          <div>
            <h3 className="text-lg font-semibold text-git-text-primary mb-2">File Format Requirements</h3>
            <div className="ml-6">
              <p className="mb-3">Your grading sheet must include the following columns:</p>
              <ul className="list-disc list-inside ml-4 space-y-1 mb-4">
                <li><strong>Student Name:</strong> Full name of the student</li>
                <li><strong>Email:</strong> Student's email address (used for matching)</li>
                <li><strong>Git Username:</strong> GitHub/GitLab username</li>
                <li><strong>Student ID:</strong> Unique identifier for the student</li>
                <li><strong>Assessment Task:</strong> Name or code of the assessment</li>
                <li><strong>Expected Contribution:</strong> Expected percentage or absolute contribution</li>
              </ul>

              <h4 className="font-semibold text-git-text-primary mb-2">Supported File Formats</h4>
              <ul className="list-disc list-inside ml-4 space-y-1">
                <li>CSV (.csv)</li>
                <li>Excel (.xlsx, .xls)</li>
                <li>Tab-delimited text (.tsv)</li>
              </ul>
            </div>
          </div>

          <div>
            <h3 className="text-lg font-semibold text-git-text-primary mb-2">How to Upload</h3>
            <div className="ml-6">
              <ol className="list-decimal list-inside ml-4 space-y-2">
                <li>Navigate to the Scaling view for your repository</li>
                <li>Click the "Upload Grading Sheet" button</li>
                <li>Select your formatted CSV or Excel file</li>
                <li>Review the preview and mapping suggestions</li>
                <li>Confirm the upload to apply the grading sheet</li>
              </ol>
            </div>
          </div>

          <div>
            <h3 className="text-lg font-semibold text-git-text-primary mb-2">Download Template</h3>
            <p className="ml-6 mb-3">
              Download a pre-formatted template to ensure your grading sheet meets all requirements:
            </p>
            <div className="ml-6">
              <button className="px-4 py-2 bg-git-int-primary hover:bg-git-int-primary-hover text-git-int-text rounded-lg transition-colors">
                Download Grading Sheet Template
              </button>
            </div>
          </div>

          <div className="mt-6 p-4 bg-blue-50 dark:bg-blue-900/20 border border-blue-200 dark:border-blue-800 rounded-lg">
            <p className="text-blue-800 dark:text-blue-200 text-sm">
              <strong>Tip:</strong> Grading sheets are stored per-repository and can be updated at any time.
              Changes will be reflected in scaling calculations immediately.
            </p>
          </div>
        </div>
      </div>
    </div>
  );
};

export default GradingSheetTab;
