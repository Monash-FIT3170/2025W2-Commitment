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
      <div className="flex justify-center mt-6">
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
=======
import React, { useState, ChangeEvent } from "react";
import { Button } from "../ui/button";

interface GradingSheetFormProps {
  onSubmit: (file?: File | null) => void;
}

export const GradingSheetForm: React.FC<GradingSheetFormProps> = ({
  onSubmit,
}) => {
  const [file, setFile] = useState<File | null>(null);

  const handleFileChange = (e: ChangeEvent<HTMLInputElement>) => {
    if (e.target.files && e.target.files.length > 0) {
      setFile(e.target.files[0]);
    }
  };

  const handleSubmit = () => {
    onSubmit(file); // will be null if no file is provided
  };

  return (
    <div className="space-y-4">
      <div>
        <label className="block text-sm font-medium text-gray-700">
          Upload Grading Sheet (optional)
        </label>
        <input
          type="file"
          accept=".csv,.xlsx,.xls"
          onChange={handleFileChange}
          className="mt-1 block w-full"
        />
      </div>
      <Button
        className="bg-git-int-primary text-git-int-text hover:bg-git-int-primary-hover"
        onClick={handleSubmit}
      >
        Submit
      </Button>
    </div>
  );
};
