import React, { useState } from "react";

import {
  Form,
  FormField,
  FormItem,
  FormLabel,
  FormControl,
  FormMessage,
  FormDescription,
} from "@ui/components/ui/form";

import { Checkbox } from "@ui/components/ui/checkbox";
import { RadioGroup, RadioGroupItem } from "@ui/components/ui/radio-group";
import { Button } from "@ui/components/ui/button";
import { z } from "zod";
import { useForm } from "react-hook-form";
import { zodResolver } from "@hookform/resolvers/zod";
import { Dropzone, DropzoneContent, DropzoneEmptyState } from '../ui/dropzone';


const scalingConfigSchema = z.object({
  metrics: z.array(z.string()).min(1, "Select at least one metric"),
  method: z.string().nonempty("Select a method"),
  customScript: z.any().optional(),
});

type ScalingConfig = z.infer<typeof scalingConfigSchema>;

export function ScalingConfigForm({
  onSubmit,
}: {
  onSubmit: (config: ScalingConfig) => void;
}) {
  const [script, setScript] = useState<File[] | undefined>();

  const form = useForm<ScalingConfig>({
    resolver: zodResolver(scalingConfigSchema),
    defaultValues: {
      metrics: [],
      method: "Percentiles",
      customScript: script,
    },
  });

  // Handle drop of files in the dropzone
  const handleDrop = (files: File[]) => {
    console.log(files);
    setScript(files);
  };

  const handleSubmit = (data: ScalingConfig) => {
    onSubmit(data);
  };

  const metricOptions = [
    "Total No. Commits",
    "Use AI to filter out commits",
    "LOC",
    "LOC per commit",
    "Commits per day",
  ];

  const methodOptions = ["Percentiles", "Mean +/- Std", "Quartiles"];

  return (
    <div className="max-w-full">
      <Form {...form}>
        <div className="text-2xl font-bold mb-4 text-center">
          Generate Scaling
        </div>
        <form onSubmit={form.handleSubmit(handleSubmit)} className="space-y-6">
          {/* Metrics Checkboxes */}
          <FormField
            control={form.control}
            name="metrics"
            render={() => (
              <FormItem>
                <FormLabel className="font-bold justify-center">
                  Select scaling metrics
                  <span className="text-red-500">*</span>
                </FormLabel>
                <div className="flex flex-col gap-2">
                  {metricOptions.map((metric) => (
                    <FormField
                      key={metric}
                      control={form.control}
                      name="metrics"
                      render={({ field }) => (
                        <FormItem
                          key={metric}
                          className="flex items-center space-x-2"
                        >
                          <FormControl>
                            <Checkbox
                              checked={field.value?.includes(metric)}
                              onCheckedChange={(checked) => {
                                const value = field.value || [];
                                return checked
                                  ? field.onChange([...value, metric])
                                  : field.onChange(
                                      value.filter((v) => v !== metric)
                                    );
                              }}
                            />
                          </FormControl>
                          <FormLabel className="font-normal">
                            {metric}
                          </FormLabel>
                        </FormItem>
                      )}
                    />
                  ))}
                </div>
                <FormMessage />
              </FormItem>
            )}
          />

          {/* Method Radios */}
          <FormField
            control={form.control}
            name="method"
            render={({ field }) => (
              <FormItem>
                <FormLabel className="font-bold justify-center">
                  Select a scaling method
                  <span className="text-red-500">*</span>
                </FormLabel>{" "}
                <FormControl>
                  <RadioGroup
                    onValueChange={field.onChange}
                    defaultValue={field.value}
                  >
                    {methodOptions.map((m) => (
                      <FormItem
                        key={m}
                        className="flex items-center space-x-2 mt-1"
                      >
                        <FormControl>
                          <RadioGroupItem value={m} />
                        </FormControl>
                        <FormLabel className="font-normal">{m}</FormLabel>
                      </FormItem>
                    ))}
                  </RadioGroup>
                </FormControl>
                <FormMessage />
              </FormItem>
            )}
          />

          {/* File Upload */}
          <FormField
            control={form.control}
            name="customScript"
            render={({ field }) => (
              <FormItem>
                <FormLabel className="font-bold justify-center">
                  Different Scale? Upload a custom script:
                </FormLabel>
                <FormControl>
                  <Dropzone
                    onDrop={handleDrop}
                    onError={console.error}
                    src={script}
                  >
                    <DropzoneEmptyState>
                      <div className="flex w-full items-center gap-4 p-8">
                        <div className="text-left">
                          <p className="font-medium text-sm">Upload a file</p>
                          <p className="text-muted-foreground text-xs">
                            Drag and drop or click to upload
                          </p>
                        </div>
                      </div>
                    </DropzoneEmptyState>
                    <DropzoneContent />
                  </Dropzone>
                </FormControl>
                <FormDescription className="justify-center">
                  Only `.py` files accepted.
                </FormDescription>
                <FormMessage />
              </FormItem>
            )}
          />

          <Button
            type="submit"
            className="bg-git-card-primary hover:bg-git-card-secondary"
          >
            Next
          </Button>
        </form>
      </Form>
    </div>
  );
}
