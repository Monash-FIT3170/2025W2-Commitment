import React, { useState, ChangeEvent } from "react";
import { Button } from "../ui/button";

interface GradingSheetFormProps {
  onSubmit: (file: File) => void;
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
    if (file) {
      onSubmit(file);
    } else {
      alert("Please upload a file before submitting.");
    }
  };

  return (
    <div className="space-y-4">
      <div>
        <label className="block text-sm font-medium text-gray-700">
          Upload Grading Sheet
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
