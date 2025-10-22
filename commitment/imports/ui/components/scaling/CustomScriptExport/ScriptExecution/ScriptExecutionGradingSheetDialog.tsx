import React from 'react';
import { Dialog, DialogContent } from "@base/dialog";
import GradingSheetForm from "@ui/components/scaling/GradingSheetForm";
import type {GradingSheetRow, ParseResult} from "@ui/components/utils/GradingSheetParser";

export interface ScriptExecutionGradingSheetDialogProps {
  open: boolean,
  onOpenChange: (open: boolean) => void,
  handleSheetSubmit?: (
    gradingSheet: File,
    parsedData?: GradingSheetRow[],
    parseResult?: ParseResult
  ) => void;
}

export default function ScriptExecutionGradingSheetDialog(props: ScriptExecutionGradingSheetDialogProps) {
  return (
    <Dialog
      open={props.open}
      onOpenChange={props.onOpenChange}
    >
      <DialogContent className="max-w-2xl">
        <GradingSheetForm
          onSubmit={props.handleSheetSubmit ?? (() => {})}
          onSkip={() => {
            props.onOpenChange(false);
          }}
        />
      </DialogContent>
    </Dialog>
  )
}