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
import { useLocation } from "react-router-dom";
import { DateRange } from "react-day-picker";
import { scaleUsers } from "./ScalingFunctions";
import { RepositoryData, FilteredData } from "/imports/api/types";
import { deserializeRepoData } from "/imports/api/serialisation";

const scalingConfigSchema = z.object({
  metrics: z.array(z.string()).min(1, "Select at least one metric"),
  method: z.string().nonempty("Select a method"),
  customScript: z.any().optional(),
});

type ScalingConfig = z.infer<typeof scalingConfigSchema>;

function ScalingConfigForm({
  onSubmit,
}: {
  onSubmit: (config: ScalingConfig) => void;
}) {
  const location = useLocation();
  const repoUrl: string | null =
    "https://github.com/AmyTjea/test_repo_for_3170";

  const [script, setScript] = useState<File[] | undefined>();
  const [metricsData, setMetricsData] = useState<RepositoryData | null>(null);
  const [dateRange, setDateRange] = useState<DateRange | undefined>();
  const [selectedBranch, setSelectedBranch] = useState<string | undefined>();
  const [selectedContributors, setSelectedContributors] = useState<string[]>(
    []
  );
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  const form = useForm<ScalingConfig>({
    resolver: zodResolver(scalingConfigSchema),
    defaultValues: {
      metrics: [],
      method: "Percentiles",
    },
  });

  const handleDrop = (files: File[]) => {
    console.log(files);
    setScript(files);
  };

  const fetchAnalyticsData = useCallback(() => {
    if (!repoUrl) return;

    setLoading(true);
    setError(null);

    Meteor.call(
      "repo.getFilteredData",
      {
        daysBack: 1000,
        repoUrl,
        branch: selectedBranch ?? "main",
        contributors: selectedContributors,
      },
      (err: Error, filtered: FilteredData) => {
        if (err) {
          setError(err.message);
          setLoading(false);
          return;
        }
        const repoData: RepositoryData = deserializeRepoData(
          filtered.repositoryData
        );
        console.log("AFTER DESERIALIZE", repoData.allCommits);

        setMetricsData(repoData);
        setDateRange({
          from: filtered.dateRange.start,
          to: filtered.dateRange.end,
        });
        setLoading(false);
      }
    );
  }, [repoUrl, selectedBranch, selectedContributors]);

  useEffect(() => {
    fetchAnalyticsData();
  }, [fetchAnalyticsData]);

  const handleSubmit = (data: ScalingConfig) => {
    if (metricsData) {
      console.log("Scaled results:", metricsData);
      const result = scaleUsers(metricsData, data);
      console.log("Scaled results:", result);
    }

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

  if (loading) return <div>Loading repo data...</div>;
  if (error) return <div>Error: {error}</div>;
  if (!metricsData) return <div>No repo data available</div>;

  return (
    <div className="w-full">
      <div className="absolute top-2 left-2 flex space-x-1">
        <span className="w-2 h-2 rounded-full bg-[#F1502F]/50" />
        <span className="w-2 h-2 rounded-full bg-[#F1502F]/30" />
      </div>
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
                        <FormItem className="flex items-center space-x-2">
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

          {/* METHOD RADIO BUTTONS */}
          <FormField
            control={form.control}
            name="method"
            render={({ field }) => (
              <FormItem>
                <FormLabel className="font-bold justify-center">
                  Select a scaling method
                  <span className="text-red-500">*</span>
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

          {/* NEXT BUTTON */}
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
