"use client";

import * as React from "react";
import { addDays, format, parse } from "date-fns";
import { CalendarIcon } from "lucide-react";
import { type DateRange } from "react-day-picker";

import { cn } from "@ui/lib/utils";
import { Button } from "@ui/components/ui/button";
import { Calendar } from "@ui/components/ui/calendar";
import {
  Popover,
  PopoverContent,
  PopoverTrigger,
} from "@ui/components/ui/popover";
import "react-day-picker/dist/style.css";

type Props = Omit<
  React.HTMLAttributes<HTMLDivElement>,
  "onChange" | "defaultValue"
> & {
  onChange?: (range: DateRange | undefined) => void;
  defaultValue?: DateRange;
};

export function DatePicker({ className, onChange, defaultValue }: Props) {
  const [date, setDate] = React.useState<DateRange | undefined>(
    defaultValue ?? {
      from: addDays(new Date(), -20),
      to: new Date(),
    }
  );

  const [fromInput, setFromInput] = React.useState<string>(
    date?.from ? format(date.from, "yyyy-MM-dd") : ""
  );
  const [toInput, setToInput] = React.useState<string>(
    date?.to ? format(date.to, "yyyy-MM-dd") : ""
  );

  // update state change
  React.useEffect(() => {
    const parsedFrom = parse(fromInput, "yyyy-MM-dd", new Date());
    const parsedTo = parse(toInput, "yyyy-MM-dd", new Date());

    if (
      !isNaN(parsedFrom.getTime()) &&
      (!toInput || !isNaN(parsedTo.getTime()))
    ) {
      const newRange: DateRange = {
        from: parsedFrom,
        to: toInput ? parsedTo : undefined,
      };
      setDate(newRange);
      onChange?.(newRange);
    }
  }, [fromInput, toInput]);

  // Update inputs
  const handleCalendarSelect = (range: DateRange | undefined) => {
    setDate(range);
    setFromInput(range?.from ? format(range.from, "yyyy-MM-dd") : "");
    setToInput(range?.to ? format(range.to, "yyyy-MM-dd") : "");
    onChange?.(range);
  };

  return (
    <div className={cn("grid gap-2", className)}>
      <Popover>
        <PopoverTrigger asChild>
          <Button
            id="date"
            variant="outline"
            className={cn(
              "w-[300px] justify-start text-left font-normal",
              !date && "text-muted-foreground"
            )}
          >
            <CalendarIcon className="mr-2 h-4 w-4" />
            {date?.from ? (
              date.to ? (
                <>
                  {format(date.from, "LLL dd, y")} -{" "}
                  {format(date.to, "LLL dd, y")}
                </>
              ) : (
                format(date.from, "LLL dd, y")
              )
            ) : (
              <span>Pick a date</span>
            )}
          </Button>
        </PopoverTrigger>
        <PopoverContent
          className="w-auto p-4 border-2"
          style={{ borderColor: "#252522" }}
          align="start"
        >
          {/* user input fields */}
          <div className="flex gap-4 mb-4">
            <div className="flex flex-col">
              <label className="text-xs mb-1 text-gray-600">From</label>
              <input
                type="date"
                value={fromInput}
                onChange={(e) => setFromInput(e.target.value)}
                className="border rounded px-2 py-1 text-sm"
              />
            </div>
            <div className="flex flex-col">
              <label className="text-xs mb-1 text-gray-600">To</label>
              <input
                type="date"
                value={toInput}
                onChange={(e) => setToInput(e.target.value)}
                className="border rounded px-2 py-1 text-sm"
              />
            </div>
          </div>

          {/* calendar selection */}
          <Calendar
            initialFocus
            mode="range"
            defaultMonth={date?.from}
            selected={date}
            onSelect={handleCalendarSelect}
            numberOfMonths={2}
            disabled={(date) => date > new Date()}
            captionLayout="dropdown"
            fromYear={2015}
            toYear={new Date().getFullYear()}
            classNames={{
              day_selected: "bg-[#F1502F] text-white hover:bg-[#F1502F]",
              day_range_middle: "bg-[#F1502F]/30 text-black",
              day_range_start:
                "rounded-l-md bg-[#F1502F] text-white hover:bg-[#F1502F]",
              day_range_end:
                "rounded-r-md bg-[#F1502F] text-white hover:bg-[#F1502F]",
              caption_dropdowns: "flex gap-2",
              caption_label: "text-sm font-medium",
              dropdown: "px-2 py-1 border rounded-md text-sm",
            }}
          />
        </PopoverContent>
      </Popover>
    </div>
  );
}
