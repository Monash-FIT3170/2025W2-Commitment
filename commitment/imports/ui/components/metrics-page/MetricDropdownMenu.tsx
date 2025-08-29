import * as React from "react";
import { Button } from "@ui/components/ui/button";
import {
  DropdownMenu,
  DropdownMenuCheckboxItem,
  DropdownMenuContent,
  DropdownMenuTrigger,
} from "@ui/components/ui/dropdown-menu";

interface DropdownMenuCheckboxesProps {
  metrics: string[];
  selected: string | undefined;
  onChange: (selected: string) => void;
}

export default function MetricDropdownMenu({
  metrics,
  selected,
  onChange,
}: DropdownMenuCheckboxesProps): React.JSX.Element {
  return (
    <DropdownMenu>
      <DropdownMenuTrigger asChild>
        <Button
          variant="outline"
          className="w-[280px] justify-start focus:outline-hidden focus:ring-0 border-2 "
          style={{ borderColor: "#35353140" }}
        >
          {selected ?? "Select a metric"}
        </Button>
      </DropdownMenuTrigger>
      <DropdownMenuContent
        className="w-[280px]  focus:ring-0 border-2"
        style={{ borderColor: "#252522" }}
      >
        {/* <DropdownMenuLabel>Select Branch</DropdownMenuLabel> */}
        {/* <DropdownMenuSeparator /> */}
        {metrics.map((metric) => (
          <DropdownMenuCheckboxItem
            key={metric}
            checked={selected === metric}
            onCheckedChange={() => onChange(metric)}
          >
            {metric}
          </DropdownMenuCheckboxItem>
        ))}
      </DropdownMenuContent>
    </DropdownMenu>
  );
}
