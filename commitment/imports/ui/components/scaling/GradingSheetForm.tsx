import React, { useState } from "react";
import { CheckCheck, CircleXIcon, LoaderCircle, UploadIcon } from "lucide-react";

import { Alert, AlertDescription } from "@ui/components/ui/alert";
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
        const result = await GradingSheetParserService.parseGradingSheet(gradingSheet[0]);
        
        if (result.success && result.data) {
          console.log("Successfully parsed");
          
          if (result.warnings && result.warnings.length > 0) {
            console.warn("Parsing warnings:", result.warnings);
          }
        } else {
          console.error("Parsing failed:", result.error);
        }
        
        setParseResult(result);
      } catch (error) {
        console.error("Unexpected parsing error:", error);
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
      onSubmit(sheet[0], parseResult?.data);
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
        <Alert className="mt-4 border-blue-200 bg-blue-50">
          <LoaderCircle className="h-4 w-4 animate-spin" />
          <AlertDescription className="text-blue-700">
            Parsing CSV file...
          </AlertDescription>
        </Alert>
      )}
      
      {parseResult?.success === false && (
        <Alert className="mt-4 border-red-200 bg-red-50" variant="destructive">
          <CircleXIcon className="h-4 w-4" />
          <AlertDescription>
            {parseResult.error}
          </AlertDescription>
        </Alert>
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