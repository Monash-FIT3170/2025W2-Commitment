import {useToast} from "@hook/useToast";
import React from "react";


/**
 * Returns an error message for invalid files uploaded, or null for valid files
 * @param file
 */
function getFileErrorMessage(file: File) : string | null {
  if (!file)
    return "No file selected";

  if (!file.name.toLowerCase().endsWith(".py"))
    return `File \"${file.name}\" must be a Python 3 script (.py)`;

  return null;
}


export default function useScriptDropzone(onUpload: (files: File[]) => void) {
  const { toast } = useToast();

  // File drop handler
  const handleDrop = (files: File[]) => {
    const errorMessages = files
      .map(file => getFileErrorMessage(file))

    // Show any error messages
    errorMessages
      .filter(msg => msg !== null)
      .forEach(msg => toast({
        title: "Error!",
        description: msg,
        variant: "destructive",
      }))

    // Continue with the valid files
    const validFiles = files
      .filter((_, i) => errorMessages[i] === null);

    if (validFiles.length > 0)
      onUpload?.(validFiles);
  };

  const onDrop = (e: React.DragEvent<HTMLDivElement>) => {
    e.preventDefault();
    const droppedFiles = Array.from(e.dataTransfer.files);
    handleDrop(droppedFiles);
  }

  const onDragOver = (e: React.DragEvent<HTMLDivElement>) => {
    e.preventDefault();
  };

  return {
    onDrop,
    handleDrop,
    onDragOver
  };
}