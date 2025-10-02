"use client";

import React, { useState } from "react";
import { z } from "zod";
import { useForm } from "react-hook-form";
import { zodResolver } from "@hookform/resolvers/zod";
import { UploadIcon, LoaderCircle, CircleXIcon } from "lucide-react";

import {
  Form,
  FormField,
  FormItem,
  FormLabel,
  FormControl,
  FormMessage,
} from "@base/form";
import { Button } from "@base/button";
import { Alert, AlertDescription } from "@base/alert";
import { Dropzone, DropzoneContent, DropzoneEmptyState } from "@base/dropzone";
import {
  GradingSheetParserService,
  type GradingSheetRow,
  type ParseResult,
} from "../utils/GradingSheetParser";

// Schema: Accept 0 or 1 file
const gradingSheetSchema = z.object({
  sheet: z.array(z.instanceof(File)).max(1).optional(),
});

type GradingSheetSchema = z.infer<typeof gradingSheetSchema>;

function GradingSheetForm({
  onSubmit,
  onSkip,
}: {
  onSubmit: (
    gradingSheet: File,
    parsedData?: GradingSheetRow[],
    parseResult?: ParseResult
  ) => void;
  onSkip?: () => void;
}): JSX.Element {
  // CSV parsing state management
  const [parseResult, setParseResult] = useState<ParseResult | null>(null);
  const [isLoading, setIsLoading] = useState(false);

  // Keep the Zod form integration (from the other branch)
  const form = useForm<GradingSheetSchema>({
    resolver: zodResolver(gradingSheetSchema),
    defaultValues: {
      sheet: [],
    },
  });

  // File drop handler with CSV validation and parsing
  const handleDrop = async (files: File[]) => {
    // Zod form integration
    form.setValue("sheet", files, {
      shouldValidate: true,
    });

    // Validate file type is CSV
    if (files[0] && !files[0].name.toLowerCase().endsWith(".csv")) {
      setParseResult({
        success: false,
        error: "Please upload a CSV file only",
      });
      return;
    }

    setParseResult(null);

    // Parse CSV file immediately when dropped using Papa Parse
    if (files[0]) {
      setIsLoading(true);
      try {
        const result = await GradingSheetParserService.parseGradingSheet(
          files[0]
        );
        setParseResult(result);
      } catch {
        setParseResult({
          success: false,
          error: "Unexpected error during parsing",
        });
      } finally {
        setIsLoading(false);
      }
    }
  };

  // Form submission handler integrating CSV parsing with parent component
  const handleFormSubmit = (data: GradingSheetSchema) => {
    const file = data.sheet?.[0];
    if (file) {
      try {
        onSubmit(file, parseResult?.data, parseResult ?? undefined);
      } catch {
        setParseResult({
          success: false,
          error: "Error submitting form",
        });
      }
    }
  };

  return (
    <div className="w-full">

      <Form {...form}>
        <div className="text-2xl font-bold mb-4 text-center">
          Upload Grading Sheet
        </div>
        <p className="text-sm text-center mb-4">
          Want to upload a scaling sheet?
        </p>
        <p className="text-sm text-muted-foreground text-center mb-4">
          We&apos;ll update the sheet to apply the scaling.
        </p>

        <form
          onSubmit={(e) => {
            e.preventDefault();
            void form.handleSubmit(handleFormSubmit)(e);
          }}
          className="space-y-6"
        >
          <FormField
            control={form.control}
            name="sheet"
            render={({ field }) => (
              <FormItem>
                <FormLabel className="font-bold justify-center">
                  Upload your grading sheet file
                </FormLabel>
                <FormControl>
                  <Dropzone
                    onDrop={(files) => {
                      void handleDrop(files);
                      field.onChange(files);
                    }}
                    onError={() => {
                      setParseResult({
                        success: false,
                        error: "Error uploading file",
                      });
                    }}
                    src={field.value}
                    accept={{ "text/csv": [".csv"] }}
                    maxFiles={1}
                    className="border-2 border-dashed border-muted-foreground rounded-md transition-colors hover:border-primary focus:border-primary"
                  >
                    <DropzoneEmptyState>
                      <div className="flex flex-col items-center w-full py-4">
                        <UploadIcon
                          size={32}
                          className="mb-2 text-muted-foreground"
                        />
                        <div className="text-center w-full">
                          <p className="font-medium text-sm mb-1">
                            Upload your grading sheet file
                          </p>
                          <p className="text-muted-foreground text-xs mb-0.5">
                            Drag and drop or click to select
                          </p>
                          <p className="text-muted-foreground text-xs">
                            File format accepted: .csv
                          </p>
                        </div>
                      </div>
                    </DropzoneEmptyState>
                    <DropzoneContent />
                  </Dropzone>
                </FormControl>
                <FormMessage />
              </FormItem>
            )}
          />

          {/* Keep your parsing status display */}
          {isLoading && (
            <Alert className="border-blue-200 bg-blue-50">
              <LoaderCircle className="h-4 w-4 animate-spin" />
              <AlertDescription className="text-blue-700">
                Parsing CSV file...
              </AlertDescription>
            </Alert>
          )}

          {parseResult?.success === false && (
            <Alert className="border-red-200 bg-red-50" variant="destructive">
              <CircleXIcon className="h-4 w-4" />
              <AlertDescription>{parseResult.error}</AlertDescription>
            </Alert>
          )}

          <div className="flex justify-center mt-6 gap-4">
            <Button
              type="submit"
              disabled={!form.watch("sheet")?.length || isLoading}
              className="bg-git-int-primary text-git-int-text hover:bg-git-int-primary-hover rounded-full px-8"
            >
              {isLoading ? "Processing..." : "Generate"}
            </Button>

            {onSkip && (
              <Button
                type="button"
                onClick={onSkip}
                variant="outline"
                className="rounded-full px-8"
              >
                Skip
              </Button>
            )}
          </div>
        </form>
      </Form>
    </div>
  );
}

export default GradingSheetForm;
