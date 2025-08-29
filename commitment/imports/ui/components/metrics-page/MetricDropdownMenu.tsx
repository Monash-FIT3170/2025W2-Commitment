import * as React from "react";
import { Button } from "@ui/components/ui/button";
import {
  DropdownMenu,
  DropdownMenuCheckboxItem,
  DropdownMenuContent,
  DropdownMenuLabel,
  DropdownMenuSeparator,
  DropdownMenuTrigger,
} from "@ui/components/ui/dropdown-menu";

import { ScrollArea } from "@ui/components/ui/scroll-area";

interface DropdownMenuCheckboxesProps {
  metrics: string[];
  selected: string[];
  onChange: (selected: string[]) => void;
}
const defaultMetric = "All Metrics"; 


export function MetricDropdownMenu({
  metrics,
  selected,
  onChange,
}: DropdownMenuCheckboxesProps): React.JSX.Element {
  const allSelected =
    selected.length === metrics.length && metrics.length > 0;

  const maxDisplayCount = 5;

  const buttonText = () => {
    if (allSelected) return "All Metrics";
    if (selected.length === 0) return defaultMetric;
    if (selected.length > maxDisplayCount) {
      const displayed = selected.slice(0, maxDisplayCount).join(", ");
      return `${displayed}, ...`;
    }
    return selected.join(", ");
  };

  return (
    <DropdownMenu>
      <DropdownMenuTrigger asChild>
        <Button
          variant="outline"
          className="w-auto justify-start focus:outline-hidden focus:ring-0 border-2"
        >
          {buttonText()}
        </Button>
      </DropdownMenuTrigger>
      <DropdownMenuContent className="w-[280px] focus:outline-hidden focus:ring-0">
        <DropdownMenuLabel>Select Metrics</DropdownMenuLabel>
        <DropdownMenuSeparator />
        <ScrollArea className="h-48">
          {metrics.map((metrics) => (
            <DropdownMenuCheckboxItem
              onSelect={(event) => event.preventDefault()}
              key={metrics}
              checked={selected.includes(metrics)}
              onCheckedChange={(checked) => {
                if (checked) {
                  onChange([...selected, metrics]);
                } else {
                  onChange(selected.filter((c) => c !== metrics));
                }
              }}
            >
              {metrics}
            </DropdownMenuCheckboxItem>
          ))}
        </ScrollArea>
      </DropdownMenuContent>
    </DropdownMenu>
  );
}
