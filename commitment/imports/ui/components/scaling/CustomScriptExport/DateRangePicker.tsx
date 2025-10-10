/** This is the same date picker as the one in the metrics page */

"use client";

import * as React from "react";
import { addDays, format, parse, subMonths } from "date-fns";
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

type Props = {
  onChange?: (range: DateRange | undefined) => void;
  defaultValue?: DateRange;
  className?: string;
};

export function DateRangePicker({ onChange, defaultValue, className }: Props) {
  const [date, setDate] = React.useState<DateRange | undefined>(
    defaultValue ?? {
      from: addDays(new Date(), -30),
      to: new Date(),
    }
  );

  // constant to set 'from' date
  const [fromInput, setFromInput] = React.useState<string>(
    date?.from ? format(date.from, "yyyy-MM-dd") : ""
  );

  // constant to set 'to' date
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
  }, [fromInput, toInput, onChange]);

  // Update inputs
  const handleCalendarSelect = (range: DateRange | undefined) => {
    setDate(range);
    setFromInput(range?.from ? format(range.from, "yyyy-MM-dd") : "");
    setToInput(range?.to ? format(range.to, "yyyy-MM-dd") : "");
    onChange?.(range);
  };

  const last30Days = () => {
    const to = new Date();
    const from = addDays(to, -30);
    const range = { from, to };
    setDate(range);
    setFromInput(format(from, "yyyy-MM-dd"));
    setToInput(format(to, "yyyy-MM-dd"));
    onChange?.(range);
  };

  const lastMonth = () => {
    const to = new Date();
    const from = subMonths(to, 1);
    const range = { from, to };
    setDate(range);
    setFromInput(format(from, "yyyy-MM-dd"));
    setToInput(format(to, "yyyy-MM-dd"));
    onChange?.(range);
  };

  const lastWeek = () => {
    const to = new Date();
    const from = addDays(to, -7);
    const range = { from, to };
    setDate(range);
    setFromInput(format(from, "yyyy-MM-dd"));
    setToInput(format(to, "yyyy-MM-dd"));
    onChange?.(range);
  };

  const last3Months = () => {
    const to = new Date();
    const from = subMonths(to, 3);
    const range = { from, to };
    setDate(range);
    setFromInput(format(from, "yyyy-MM-dd"));
    setToInput(format(to, "yyyy-MM-dd"));
    onChange?.(range);
  };

  const clearDates = () => {
    setDate(undefined);
    setFromInput("");
    setToInput("");
    onChange?.(undefined);
  };

  return (
    <div className={cn("grid gap-2", className)}>
      <Popover>
        <PopoverTrigger asChild>
          <Button
            id="date"
            variant="outline"
            className={cn(
              "w-full justify-start text-left font-normal border-2 rounded-lg border-git-stroke-primary/40"
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
              <span>Pick a date range</span>
            )}
          </Button>
        </PopoverTrigger>
        <PopoverContent
          className="w-auto p-4 border-2 border-git-stroke-primary/40"
          align="start"
        >
          <div className="flex flex-wrap gap-2 mb-4 justify-center items-center">
            <Button size="sm" variant="outline" onClick={lastWeek}>
              Last 7 Days
            </Button>
            <Button size="sm" variant="outline" onClick={last30Days}>
              Last 30 Days
            </Button>
            <Button size="sm" variant="outline" onClick={lastMonth}>
              Last Month
            </Button>
            <Button size="sm" variant="outline" onClick={last3Months}>
              Last 3 Months
            </Button>
            <Button
              size="sm"
              variant="ghost"
              onClick={clearDates}
              disabled={!date?.from && !date?.to}
            >
              Clear
            </Button>
          </div>

          {/* calendar selection */}
          <Calendar
            initialFocus
            mode="range"
            defaultMonth={subMonths(new Date(), 1)}
            selected={date}
            onSelect={handleCalendarSelect}
            numberOfMonths={2}
            disabled={(day) => {
              const today = new Date();
              return day > today;
            }}
            captionLayout="dropdown"
            fromYear={2015}
            toYear={new Date().getFullYear()}
            classNames={{
              day_today: "text-black font-normal",
              day_selected:
                "bg-git-int-primary text-white hover:bg-git-int-primary",
              day_range_middle: "bg-git-int-primary/50 text-black",
              day_range_start:
                "rounded-l-md bg-git-int-primary text-white hover:bg-git-int-primary",
              day_range_end:
                "rounded-r-md bg-git-int-primary text-white hover:bg-git-int-primary",
              caption_dropdowns: "flex gap-2",
              caption_label: "text-sm font-medium text-black",
              dropdown:
                "px-2 py-1 border-1 border-git-stroke-primary rounded-md text-sm text-black",
            }}
          />
        </PopoverContent>
      </Popover>
    </div>
  );
}
