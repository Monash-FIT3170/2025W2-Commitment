import React, { useRef } from "react";
import { Button } from "@base/button";
import {
  AlertDialog,
  AlertDialogTrigger,
  AlertDialogContent,
  AlertDialogHeader,
  AlertDialogTitle,
  AlertDialogDescription,
  AlertDialogFooter,
  AlertDialogCancel,
  AlertDialogAction,
} from "@base/alert-dialog";

// tool bar for the upload json and template json
type Props = {
  helpOpen: boolean;
  setHelpOpen: (v: boolean) => void;
  onUploadText: (text: string) => void;
  onError: (msg: string | null) => void;
};

// Sample regex rules
const sampleRules = [
  {
    key: "conventional_header",
    regex: "^(feat|fix|docs|chore|style|refactor|test|ci)(\\(.+\\))?:\\s",
    scale: 2,
    sign: "+",
  },
  { key: "wip_present", regex: "\\bWIP\\b", scale: 1, sign: "-" },
  { key: "merge_message", regex: "^Merge\\spull\\srequest", scale: 1, sign: "-" },
  { key: "create_title", regex: "^Create\\b", scale: 1, sign: "-" },
] as const;

export default function RulesToolbar({
  helpOpen,
  setHelpOpen,
  onUploadText,
  onError,
}: Props): React.JSX.Element {
  const fileInputRef = useRef<HTMLInputElement | null>(null);

  const handleUploadClick = () => fileInputRef.current?.click();

  const handleFileSelected: React.ChangeEventHandler<HTMLInputElement> = (e) => {
    const f = e.target.files?.[0];
    if (!f) return;

    const reader = new FileReader();
    reader.onload = () => {
      const res = reader.result;
      if (typeof res === "string") {
        onUploadText(res);
        onError(null);
      } else {
        onError("Invalid rules file (unsupported format)");
      }
    };
    reader.readAsText(f);

    // Reset input so same file can be selected again
    e.target.value = "";
  };

  const handleDownloadSample = () => {
    const blob = new Blob([JSON.stringify(sampleRules, null, 2)], {
      type: "application/json",
    });
    const url = URL.createObjectURL(blob);
    const a = document.createElement("a");
    a.href = url;
    a.download = "regex_rules_sample.json";
    document.body.appendChild(a);
    a.click();
    a.remove();
    URL.revokeObjectURL(url);
  };

  return (
    <div className="flex items-center gap-2">
      <Button variant="outline" onClick={handleUploadClick}>
        Upload Rules JSON
      </Button>
      <input
        ref={fileInputRef}
        type="file"
        accept="application/json"
        className="hidden"
        onChange={handleFileSelected}
      />
      
      {/* Buttons' behaviour */}
      <AlertDialog open={helpOpen} onOpenChange={setHelpOpen}>
        <AlertDialogTrigger asChild>
          <Button variant="outline">Help / Samples</Button>
        </AlertDialogTrigger>
        <AlertDialogContent className="max-w-2xl">
          <AlertDialogHeader>
            <AlertDialogTitle>Rules JSON Format</AlertDialogTitle>
            <AlertDialogDescription asChild>
              <div className="min-w-0">
                <p className="mb-2">Provide an array of rules. Each rule has:</p>
                <div className="h-80 overflow-auto bg-git-bg-secondary rounded">
                  <pre className="m-0 p-4 text-sm whitespace-pre-wrap break-words"> 
                    {JSON.stringify(sampleRules, null, 2)}
                  </pre>
                </div>
              </div>
            </AlertDialogDescription>
          </AlertDialogHeader>
          <AlertDialogFooter>
            <AlertDialogCancel>Close</AlertDialogCancel>
            <AlertDialogAction onClick={handleDownloadSample}>
              Download Sample
            </AlertDialogAction>
          </AlertDialogFooter>
        </AlertDialogContent>
      </AlertDialog>
    </div>
  );
}
