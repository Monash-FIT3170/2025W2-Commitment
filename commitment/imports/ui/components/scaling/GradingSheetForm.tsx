import React, { useState } from "react";

import { UploadIcon } from "lucide-react";
import { Button } from "@ui/components/ui/button";
import { Dropzone, DropzoneContent, DropzoneEmptyState } from "../ui/dropzone";
import { GradingSheetParserService, type GradingSheetRow, type ParseResult } from "../utils/GradingSheetParser";

function GradingSheetForm({
  onSubmit,
}: {
  onSubmit: (gradingSheet: File, parsedData?: GradingSheetRow[]) => void;
}) {
  const [sheet, setSheet] = useState<File[] | undefined>();
  const [parseResult, setParseResult] = useState<ParseResult | null>(null);
  const [isLoading, setIsLoading] = useState(false);

  // Handle drop of files in the dropzone
  const handleDrop = async (gradingSheet: File[]) => {
    console.log("Files dropped:", gradingSheet);
    
    // Additional validation for CSV files
    if (gradingSheet[0] && !gradingSheet[0].name.toLowerCase().endsWith('.csv')) {
      console.error('Please upload a CSV file only');
      setParseResult({
        success: false,
        error: 'Please upload a CSV file only'
      });
      return;
    }
    
    setSheet(gradingSheet);
    setParseResult(null);
    
    // Parse the CSV file immediately when dropped
    if (gradingSheet[0]) {
      setIsLoading(true);
      try {
        console.log("🔄 Starting CSV parsing for file:", gradingSheet[0].name);
        const result = await GradingSheetParserService.parseGradingSheet(gradingSheet[0]);
        
        console.log("📊 CSV Parsing Result:", result);
        
        if (result.success && result.data) {
          console.log("✅ Successfully parsed", result.rowCount, "students");
          console.log("📋 First 3 students:", result.data.slice(0, 3));
          
          // Get summary statistics
          const summary = GradingSheetParserService.getDataSummary(result.data);
          console.log("📈 Data Summary:", summary);
          
          if (result.warnings && result.warnings.length > 0) {
            console.warn("⚠️ Parsing warnings:", result.warnings);
          }
        } else {
          console.error("❌ Parsing failed:", result.error);
        }
        
        setParseResult(result);
      } catch (error) {
        console.error("💥 Unexpected parsing error:", error);
        setParseResult({
          success: false,
          error: 'Unexpected error during parsing'
        });
      } finally {
        setIsLoading(false);
      }
    }
  };

  const handleSubmit = () => {
    if (sheet?.[0]) {
      console.log("🚀 Submitting grading sheet with parsed data");
      console.log("📁 File:", sheet[0].name);
      console.log("📊 Parsed data available:", !!parseResult?.data);
      
      if (parseResult?.success && parseResult.data) {
        console.log("📋 Submitting with", parseResult.data.length, "parsed student records");
        onSubmit(sheet[0], parseResult.data);
      } else {
        console.log("📋 Submitting without parsed data (parsing failed or not completed)");
        onSubmit(sheet[0]);
      }
    }
  };

  return (
    <div className="w-full">
      <div className="absolute top-2 left-2 flex space-x-1">
        <span className="w-2 h-2 rounded-full bg-[#F1502F]/30" />
        <span className="w-2 h-2 rounded-full bg-[#F1502F]/50" />
      </div>

      <div className="text-2xl font-bold mb-4 text-center">
        Generate Scaling
      </div>
      <p className="text-sm text-center mb-4">
        Want to upload a scaling sheet?
      </p>
      <p className="text-sm text-muted-foreground text-center mb-4">
        We'll update the sheet to apply the scaling.
      </p>
      <Dropzone
        onDrop={handleDrop}
        onError={console.error}
        src={sheet}
        accept={{ ".csv": [] }}
        maxFiles={1}
        className="border-2 border-dashed border-muted-foreground rounded-md transition-colors hover:border-primary focus:border-primary"
      >
        <DropzoneEmptyState>
          <div className="flex flex-col items-center w-full py-4">
            <UploadIcon size={32} className="mb-2 text-muted-foreground" />
            <div className="text-center w-full">
              <p className="font-medium text-sm mb-1">
                Upload your grading sheet file
              </p>
              <p className="text-muted-foreground text-xs mb-0.5">
                Drag and drop or click to select
              </p>
              <p className="text-muted-foreground text-xs mb-0.5">
                File format accepted: .csv
              </p>
            </div>
          </div>
        </DropzoneEmptyState>
        <DropzoneContent />
      </Dropzone>

      {/* Parsing Status Display */}
      {isLoading && (
        <div className="mt-4 p-3 bg-blue-100 border border-blue-400 text-blue-700 rounded">
          🔄 Parsing CSV file...
        </div>
      )}
      
      {parseResult?.success === false && (
        <div className="mt-4 p-3 bg-red-100 border border-red-400 text-red-700 rounded">
          ❌ {parseResult.error}
        </div>
      )}
      
      {parseResult?.success && parseResult.data && (
        <div className="mt-4 p-3 bg-green-100 border border-green-400 text-green-700 rounded">
          ✅ CSV parsed successfully! Found {parseResult.rowCount} students.
          {parseResult.warnings && parseResult.warnings.length > 0 && (
            <div className="mt-2 text-orange-600">
              ⚠️ Warnings: {parseResult.warnings.join('; ')}
            </div>
          )}
        </div>
      )}

      {/* NEXT BUTTON */}
      <div className="flex justify-center mt-6">
        <Button
          type="button"
          onClick={handleSubmit}
          disabled={!sheet || isLoading}
          className="bg-git-int-primary text-git-int-text hover:bg-git-int-primary-hover rounded-full px-8"
        >
          {isLoading ? 'Processing...' : 'Generate'}
        </Button>
      </div>
    </div>
  );
}

export default GradingSheetForm;