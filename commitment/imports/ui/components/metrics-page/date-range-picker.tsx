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
import "react-day-picker/dist/style.css";

type Props = {
  onChange?: (range: DateRange | undefined) => void;
};

export function DatePicker({ onChange }: Props) {
  const [date, setDate] = React.useState<DateRange | undefined>();

  // constant to set 'from' date
  const [fromInput, setFromInput] = React.useState<string>(
    date?.from ? format(date.from, "yyyy-MM-dd") : ""
  );

  // constant to set 'to' date
  const [toInput, setToInput] = React.useState<string>(
    date?.to ? format(date.to, "yyyy-MM-dd") : ""
  );

  // constant to ensure 12 week selection
  const is12Weeks = (from: Date, to: Date) => {
    const maxTo = addDays(from, 84);
    return to <= maxTo;
  };

  // update state change
  React.useEffect(() => {
    const parsedFrom = parse(fromInput, "yyyy-MM-dd", new Date());
    const parsedTo = parse(toInput, "yyyy-MM-dd", new Date());

    if (
      !isNaN(parsedFrom.getTime()) &&
      (!toInput || !isNaN(parsedTo.getTime()))
    ) {
      if (parsedTo && parsedFrom && !is12Weeks(parsedFrom, parsedTo)) {
        return;
      }

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
    if (range?.from && range?.to && !is12Weeks(range.from, range.to)) {
      return;
    }
    setDate(range);
    setFromInput(range?.from ? format(range.from, "yyyy-MM-dd") : "");
    setToInput(range?.to ? format(range.to, "yyyy-MM-dd") : "");
    onChange?.(range);
  };

  const last12Weeks = () => {
    const to = new Date();
    const from = addDays(to, -84);
    const range = { from, to };
    setDate(range);
    onChange?.(range);
  };

  const lastMonth = () => {
    const to = new Date();
    const from = subMonths(to, 1);
    const range = { from, to };
    setDate(range);
    onChange?.(range);
  };

  const lastWeek = () => {
    const to = new Date();
    const from = addDays(to, -7);
    const range = { from, to };
    setDate(range);
    onChange?.(range);
  };

  return (
    <div className={cn("grid gap-2")}>
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
          <div className="flex gap-2 mb-4 justify-center items-center">
            <Button size="sm" variant="outline" onClick={last12Weeks}>
              Last 12 Weeks
            </Button>
            <Button size="sm" variant="outline" onClick={lastMonth}>
              Last Month
            </Button>
            <Button size="sm" variant="outline" onClick={lastWeek}>
              Last 7 Days
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
              if (day > today) return true;

              if (date?.from) {
                const maxTo = addDays(date.from, 84);
                return day > maxTo;
              }

              return false;
            }}
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
