
"use client";

import * as React from "react";
import { CalendarDays, Calendar } from "lucide-react";
import { ToggleGroup, ToggleGroupItem } from "../ui/toggle-group";

type Mode =  "week" | "month";

export function ModeToggle({
  value,
  onChange,
}: {
  value: Mode;
  onChange: (v: Mode) => void;
}) {
  return (
    <ToggleGroup
      type="single"
      value={value}
      onValueChange={(v) => v && onChange(v as Mode)}
      className="justify-start"
    >
      
      <ToggleGroupItem
        value="week"
        className="flex items-center gap-2 px-3 py-2"
      >
        <CalendarDays className="h-4 w-4" /> Week
      </ToggleGroupItem>
      <ToggleGroupItem
        value="month"
        className="flex items-center gap-2 px-3 py-2"
      >
        <Calendar className="h-4 w-4" /> Month
      </ToggleGroupItem>
    </ToggleGroup>
  );
}
