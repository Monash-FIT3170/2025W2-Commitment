import {useToast} from "@hook/useToast";
import {Dropzone, DropzoneContent, DropzoneEmptyState} from "@base/dropzone";
import { UploadIcon} from "lucide-react";
import React from "react";

export interface ScriptUploadProps {
  onUpload: (files: File[]) => void,
  children?: React.ReactNode,
}


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


export default function ScriptDropzone(props: ScriptUploadProps) {
  const { toast } = useToast();

  // File drop handler
  const handleDrop = async (files: File[]) => {
    const errorMessages = files
      .map(file => getFileErrorMessage(file))

    // Show any error messages
    errorMessages
      .filter(msg => msg !== null)
      .forEach(msg => toast({
        title: "Error!",
        description: msg,
      }))

    // Continue with the valid files
    const validFiles = files
      .filter((_, i) => errorMessages[i] === null);

    props.onUpload?.(validFiles);
  };

  // className="border-2 border-dashed border-muted-foreground rounded-md transition-colors hover:border-primary focus:border-primary"


  const backupChildren = (<>
    <DropzoneEmptyState>
      <div className="flex flex-col items-center w-full py-4">
        <UploadIcon
          size={32}
          className="mb-2 text-muted-foreground"
        />
        <div className="text-center w-full">
          <p className="font-medium text-sm mb-1">
            Upload your python script
          </p>
          <p className="text-muted-foreground text-xs mb-0.5">
            Drag and drop or click to select
          </p>
          <p className="text-muted-foreground text-xs">
            File format accepted: .py
          </p>
        </div>
      </div>
    </DropzoneEmptyState>
    <DropzoneContent />
  </>)
  const children = props.children ?? backupChildren;

  return (
    <Dropzone
      onDrop={(files) => {
        void handleDrop(files);
      }}
      accept={{ "text/py": [".py"] }}
      maxFiles={1}
    >
      {children}
    </Dropzone>
  )
}