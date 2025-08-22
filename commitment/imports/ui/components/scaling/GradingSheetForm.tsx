"use client";

import React from "react";
import { z } from "zod";
import { useForm } from "react-hook-form";
import { zodResolver } from "@hookform/resolvers/zod";
import { UploadIcon } from "lucide-react";

import {
  Form,
  FormField,
  FormItem,
  FormLabel,
  FormControl,
  FormMessage,
} from "@ui/components/ui/form";
import { Button } from "@ui/components/ui/button";
import { Dropzone, DropzoneContent, DropzoneEmptyState } from "../ui/dropzone";

// Schema: Accept 0 or 1 file
const gradingSheetSchema = z.object({
  sheet: z.array(z.instanceof(File)).max(1).optional(),
});

type GradingSheetSchema = z.infer<typeof gradingSheetSchema>;

function GradingSheetForm({
  onSubmit,
}: {
  onSubmit: (gradingSheet: File | null) => void;
}) {
  const form = useForm<GradingSheetSchema>({
    resolver: zodResolver(gradingSheetSchema),
    defaultValues: {
      sheet: [],
    },
  });

  const handleDrop = (files: File[]) => {
    form.setValue("sheet", files, {
      shouldValidate: true,
    });
  };

  const handleFormSubmit = (data: GradingSheetSchema) => {
    const file = data.sheet?.[0] || null;
    onSubmit(file); // Either a File or null
  };

  return (
    <div className="w-full">
      <div className="absolute top-2 left-2 flex space-x-1">
        <span className="w-2 h-2 rounded-full bg-[#F1502F]/30" />
        <span className="w-2 h-2 rounded-full bg-[#F1502F]/50" />
      </div>

      <Form {...form}>
        <div className="text-2xl font-bold mb-4 text-center">
          Upload Grading Sheet
        </div>
        <p className="text-sm text-center mb-4">
          We'll apply scaling to the sheet you upload.
        </p>

        <form
          onSubmit={form.handleSubmit(handleFormSubmit)}
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
                      handleDrop(files);
                      field.onChange(files);
                    }}
                    onError={console.error}
                    src={field.value}
                    accept={{ ".xlsx": [] }}
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
                            Upload your grading sheet file (Optional)
                          </p>
                          <p className="text-muted-foreground text-xs mb-0.5">
                            Drag and drop or click to select
                          </p>
                          <p className="text-muted-foreground text-xs">
                            Accepted: <span className="font-mono">.xlsx</span>
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

          <div className="flex justify-center mt-6">
            <Button
              type="submit"
              className="bg-git-int-primary text-git-int-text hover:bg-git-int-primary-hover rounded-full px-8"
            >
              Generate
            </Button>
          </div>
        </form>
      </Form>
    </div>
  );
}

export default GradingSheetForm;
