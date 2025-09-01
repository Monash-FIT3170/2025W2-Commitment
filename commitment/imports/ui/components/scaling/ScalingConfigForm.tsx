import React, { useState, useEffect, useCallback } from "react";
import { Meteor } from "meteor/meteor";
import {
  Form,
  FormField,
  FormItem,
  FormLabel,
  FormControl,
  FormMessage,
} from "@ui/components/ui/form";
import { UploadIcon } from "lucide-react";
import { Checkbox } from "@ui/components/ui/checkbox";
import { RadioGroup, RadioGroupItem } from "@ui/components/ui/radio-group";
import { Button } from "@ui/components/ui/button";
import { z } from "zod";
import { useForm } from "react-hook-form";
import { zodResolver } from "@hookform/resolvers/zod";
import { Dropzone, DropzoneContent, DropzoneEmptyState } from "../ui/dropzone";
import {
  RepositoryData,
  FilteredData,
  UserScalingSummary,
  SerialisableMapObject,
} from "/imports/api/types";
import { useLocation } from "react-router-dom";
import { config } from "process";

const scalingConfigSchema = z.object({
  metrics: z.array(z.string()).min(1, "Select at least one metric"),
  method: z.string().nonempty("Select a method"),
  customScript: z.any().optional(),
});

export type ScalingConfig = z.infer<typeof scalingConfigSchema>;

interface ScalingConfigFormProps {
  onSubmit: (
    config: ScalingConfig,
    scaledResults: UserScalingSummary[]
  ) => void;
}

function ScalingConfigForm({ onSubmit }: ScalingConfigFormProps) {
  const location = useLocation();
  const repoUrl: string = location.state?.repoUrl ?? null;

  const [script, setScript] = useState<File[] | undefined>();
  const [selectedBranch, setSelectedBranch] = useState<string | undefined>();
  const [selectedContributors, setSelectedContributors] = useState<string[]>(
    []
  );

  const [scaledResults, setScaledResults] = useState([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  const form = useForm<ScalingConfig>({
    resolver: zodResolver(scalingConfigSchema),
    defaultValues: {
      metrics: [],
      method: "Percentiles",
    },
  });

  const handleDrop = (files: File[]) => setScript(files);
  const handleSubmit = async (data: ScalingConfig) => {
    try {
      const result = await Meteor.callAsync("getScalingResults", data, repoUrl);

      // Update state for later renders
      setScaledResults(result);

      console.log("radssadesult", result);

      // Pass the actual result, not the outdated state
      onSubmit(data, result);
    } catch (err) {
      console.error("Error:", err);
    }
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
    <div className="w-full">
      <Form {...form}>
        <div className="text-2xl font-bold mb-4 text-center">
          Generate Scaling
        </div>
        <form onSubmit={form.handleSubmit(handleSubmit)} className="space-y-6">
          {/* METRICS CHECKBOXES */}
          <FormField
            control={form.control}
            name="metrics"
            render={() => (
              <FormItem>
                <FormLabel className="font-bold justify-center">
                  Select scaling metrics<span className="text-red-500">*</span>
                </FormLabel>
                <div className="flex flex-col gap-2">
                  {metricOptions.map((metric) => (
                    <FormField
                      key={metric}
                      control={form.control}
                      name="metrics"
                      render={({ field }) => (
                        <FormItem className="flex items-center space-x-2">
                          <FormControl>
                            <Checkbox
                              checked={field.value?.includes(metric)}
                              onCheckedChange={(checked) => {
                                const value = field.value || [];
                                field.onChange(
                                  checked
                                    ? [...value, metric]
                                    : value.filter((v) => v !== metric)
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

          {/* METHOD RADIO BUTTONS */}
          <FormField
            control={form.control}
            name="method"
            render={({ field }) => (
              <FormItem>
                <FormLabel className="font-bold justify-center">
                  Select a scaling method<span className="text-red-500">*</span>
                </FormLabel>
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

          {/* FILE UPLOAD */}
          <FormField
            control={form.control}
            name="customScript"
            render={({ field }) => (
              <FormItem>
                <FormLabel className="font-bold justify-center">
                  Custom scaling? Upload a Python script:
                </FormLabel>
                <FormControl>
                  <Dropzone
                    onDrop={(files) => {
                      handleDrop(files);
                      field.onChange(files);
                    }}
                    onError={console.error}
                    src={script}
                    accept={{ ".py": [] }}
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
                            Upload a Python file
                          </p>
                          <p className="text-muted-foreground text-xs mb-0.5">
                            Drag and drop or click to select
                          </p>
                          <p className="text-muted-foreground text-xs">
                            Accepted: <span className="font-mono">.py</span>
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

          <div className="flex justify-center">
            <Button
              type="submit"
              className="bg-git-int-primary text-git-int-text hover:bg-git-int-primary-hover rounded-full px-8"
            >
              Next
            </Button>
          </div>
        </form>
      </Form>
    </div>
  );
}

export default ScalingConfigForm;
