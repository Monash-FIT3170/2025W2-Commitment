import React, { useState } from "react";

import { UploadIcon } from "lucide-react";
import { Button } from "@ui/components/ui/button";
import { Dropzone, DropzoneContent, DropzoneEmptyState } from "../ui/dropzone";

function GradingSheetForm({
  onSubmit,
}: {
  onSubmit: (gradingSheet: File) => void;
}) {
  const [sheet, setSheet] = useState<File[] | undefined>();

  // Handle drop of files in the dropzone
  const handleDrop = (gradingSheet: File[]) => {
    console.log(gradingSheet);
    setSheet(gradingSheet);
  };

  return (
    <div className="max-w-full">
      <div className="text-2xl font-bold mb-4 text-center">
        Want to upload a grading sheet?
      </div>
      <Dropzone
        onDrop={handleDrop}
        onError={console.error}
        src={sheet}
        accept={{ ".xlsx": [] }}
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
                File format accepted: .xlsx
              </p>
            </div>
          </div>
        </DropzoneEmptyState>
        <DropzoneContent />
      </Dropzone>

      {/* NEXT BUTTON */}
      <div className="flex justify-center">
        <Button
          type="submit"
          className="bg-git-int-primary text-git-int-text hover:bg-git-int-primary-hover rounded-full px-8"
        >
          Generate
        </Button>
      </div>
    </div>
  );
}

export default GradingSheetForm;
