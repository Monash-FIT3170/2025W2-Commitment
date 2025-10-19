import React, { useState, useEffect } from "react";
import { Meteor } from "meteor/meteor";
import {
  Form,
  FormField,
  FormItem,
  FormLabel,
  FormControl,
  FormMessage,
} from "@base/form";
import { Checkbox } from "@base/checkbox";
import { RadioGroup, RadioGroupItem } from "@base/radio-group";
import { Button } from "@base/button";
import { z } from "zod";
import { useForm } from "react-hook-form";
import { zodResolver } from "@hookform/resolvers/zod";
import { useLocation } from "react-router-dom";
import { UserScalingSummary } from "/imports/api/types";

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

interface ScalingViewLocationState {
  repoUrl?: string;
}

function ScalingConfigForm({ onSubmit }: ScalingConfigFormProps) {
  const location = useLocation();

  const state = location.state as ScalingViewLocationState | null;
  const repoUrl: string | null = state?.repoUrl ?? null;

  //   Make a repo call here to get the number of contributors

  const form = useForm<ScalingConfig>({
    resolver: zodResolver(scalingConfigSchema),
    defaultValues: {
      metrics: [],
      method: "Percentiles",
    },
  });

  const handleSubmit = async (data: ScalingConfig) => {
    try {
      const result = (await Meteor.callAsync(
        "getScalingResults",
        data,
        repoUrl
      )) as UserScalingSummary[];

      onSubmit(data, result); // this is where all the scaling starts from
    } catch (err) {
      console.error("Error:", err);
    }
  };

  const [isSmallGroupCache, setIsSmallGroupCache] = useState<
    Record<string, boolean>
  >({});
  const [methodOptions, setMethodOptions] = useState<string[]>([
    "Percentiles",
    "Mean +/- Std",
    "Quartiles",
  ]);

  useEffect(() => {
    const fetchSmallGroup = async () => {
      if (!repoUrl) return;

      // check cache first
      if (isSmallGroupCache[repoUrl] !== undefined) {
        setMethodOptions(
          isSmallGroupCache[repoUrl]
            ? ["Compact Scaling"]
            : ["Percentiles", "Mean +/- Std", "Quartiles"]
        );
        return;
      }

      try {
        const result = (await Meteor.callAsync(
          "isSmallContributorGroup",
          repoUrl,
          6
        )) as boolean;

        // update cache
        setIsSmallGroupCache((prev) => ({ ...prev, [repoUrl]: result }));

        setMethodOptions(
          result
            ? ["Compact Scaling"]
            : ["Percentiles", "Mean +/- Std", "Quartiles"]
        );
      } catch (err) {
        console.error("Error checking contributor group size:", err);
      }
    };

    void fetchSmallGroup();
  }, [repoUrl, isSmallGroupCache]);

  // Automatically select the first method option whenever it changes
  useEffect(() => {
    if (methodOptions.length > 0) {
      form.setValue("method", methodOptions[0]);
    }
  }, [methodOptions, form]);

  const metricOptions = [
    "Total No. Commits",
    // "Use AI to filter out commits",
    "LOC",
    "LOC Per Commit",
    "Commits Per Day",
  ];

  return (
    <div className="w-full">
      <Form {...form}>
        <div className="text-2xl font-bold mb-4 text-center">
          Generate Scaling
        </div>
        <form
          onSubmit={(e) => {
            e.preventDefault();
            void form.handleSubmit(handleSubmit)(e);
          }}
          className="space-y-6"
        >
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
                    value={field.value}
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
          <div className="flex justify-center">
            <Button
              type="submit"
              disabled={!form.watch("method")}
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
