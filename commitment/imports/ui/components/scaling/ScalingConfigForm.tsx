import React from "react";

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
import { Input } from "@ui/components/ui/input";
import { Button } from "@ui/components/ui/button";
import { z } from "zod";
import { useForm } from "react-hook-form";
import { zodResolver } from "@hookform/resolvers/zod";

const scalingConfigSchema = z.object({
  metrics: z.array(z.string()).min(1, "Select at least one metric"),
  method: z.string().nonempty("Select a method"),
  customScript: z.any().optional(),
});

type ScalingConfig = z.infer<typeof scalingConfigSchema>;

export function ScalingConfigForm({ onSubmit }: { onSubmit: (config: ScalingConfig) => void }) {
  const form = useForm<ScalingConfig>({
    resolver: zodResolver(scalingConfigSchema),
    defaultValues: {
      metrics: ["Total No. Commits", "LOC per commit"],
      method: "Mean +/- Std",
      customScript: null,
    },
  });

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
    <Form {...form}>
      <form onSubmit={form.handleSubmit(handleSubmit)} className="space-y-6">
        {/* Metrics Checkboxes */}
        <FormField
          control={form.control}
          name="metrics"
          render={() => (
            <FormItem>
              <FormLabel>Select scaling metrics*</FormLabel>
              <div className="flex flex-wrap gap-4">
                {metricOptions.map((metric) => (
                  <FormField
                    key={metric}
                    control={form.control}
                    name="metrics"
                    render={({ field }) => (
                      <FormItem key={metric} className="flex items-center space-x-2">
                        <FormControl>
                          <Checkbox
                            checked={field.value?.includes(metric)}
                            onCheckedChange={(checked) => {
                              const value = field.value || [];
                              return checked
                                ? field.onChange([...value, metric])
                                : field.onChange(value.filter((v) => v !== metric));
                            }}
                          />
                        </FormControl>
                        <FormLabel className="font-normal">{metric}</FormLabel>
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
              <FormLabel>Select a scaling method*</FormLabel>
              <FormControl>
                <RadioGroup onValueChange={field.onChange} defaultValue={field.value}>
                  {methodOptions.map((m) => (
                    <FormItem key={m} className="flex items-center space-x-2 mt-1">
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
              <FormLabel>Upload custom script (optional)</FormLabel>
              <FormControl>
                <Input
                  type="file"
                  accept=".py"
                  onChange={(e) => field.onChange(e.target.files?.[0])}
                />
              </FormControl>
              <FormDescription>Only `.py` files accepted.</FormDescription>
              <FormMessage />
            </FormItem>
          )}
        />

        <Button type="submit" className="bg-orange-400 hover:bg-orange-500">
          Next
        </Button>
      </form>
    </Form>
  );
}