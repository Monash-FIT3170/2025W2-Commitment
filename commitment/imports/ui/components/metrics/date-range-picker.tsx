"use client";

import * as React from "react";
import { addDays, format, parse, subMonths, subWeeks } from "date-fns";
import { CalendarIcon, ChevronLeft, ChevronRight } from "lucide-react";
import { type DateRange } from "react-day-picker";
import { startOfDay } from "date-fns";

import { cn } from "@ui/lib/utils";
import { Button } from "@base/button";
import { Calendar } from "@base/calendar";
import { Popover, PopoverContent, PopoverTrigger } from "@base/popover";
// import "react-day-picker/dist/style.css";

type Props = {
  onChange?: (range: DateRange | undefined) => void;
  defaultValue?: DateRange;
};

export function DatePicker({ onChange, defaultValue }: Props) {
  const [date, setDate] = React.useState<DateRange>() ?? defaultValue;

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
    const now = new Date();

    const parsedFrom = parse(fromInput, "yyyy-MM-dd", new Date());
    const parsedToRaw =
      toInput?.trim() !== ""
        ? parse(toInput, "yyyy-MM-dd", new Date())
        : undefined;

    let parsedTo: Date | undefined = parsedToRaw;

    if (parsedTo && !isNaN(parsedTo.getTime())) {
      // Set current time on parsedTo
      parsedTo.setHours(
        now.getHours(),
        now.getMinutes(),
        now.getSeconds(),
        now.getMilliseconds()
      );
    }

    if (
      !isNaN(parsedFrom.getTime()) &&
      (!parsedTo || !isNaN(parsedTo.getTime()))
    ) {
      if (parsedFrom && parsedTo && !is12Weeks(parsedFrom, parsedTo)) {
        return;
      }

      const newRange: DateRange = {
        from: parsedFrom,
        to: parsedTo,
      };

      setDate(newRange);
      onChange?.(newRange);
    }
  }, [fromInput, toInput]);

  // Update inputs
  const handleCalendarSelect = (range: DateRange | undefined) => {
    if (range?.from && range?.to) {
      if (!is12Weeks(range.from, range.to)) {
        setWarning("You can only select up to 12 weeks.");
        return;
      }

      if (
        date?.from?.getTime() === range.from.getTime() &&
        date?.to?.getTime() === range.to.getTime()
      ) {
        clearDates();
        return;
      }
    }

    setWarning(null);
    setDate(range);
    setFromInput(range?.from ? format(range.from, "yyyy-MM-dd") : "");
    setToInput(range?.to ? format(range.to, "yyyy-MM-dd") : "");
    onChange?.(range);
  };

  const last12Weeks = () => {
    const to = new Date();
    const from = subWeeks(to, 12);
    const range = { from, to };
    setWarning(null);
    setDate(range);
    onChange?.(range);
  };

  const lastMonth = () => {
    const to = new Date();
    const from = startOfDay(subMonths(to, 1));
    const range = { from, to };
    setWarning(null);
    setDate(range);
    onChange?.(range);
  };

  const lastWeek = () => {
    const to = new Date();
    const from = startOfDay(addDays(to, -7));
    const range = { from, to };
    setWarning(null);
    setDate(range);
    onChange?.(range);
  };

  const clearDates = () => {
    setWarning(null);
    setDate(undefined);
    setFromInput("");
    setToInput("");
    onChange?.(undefined);
  };

  const [warning, setWarning] = React.useState<string | null>(null);

  const [currentMonth, setCurrentMonth] = React.useState<Date>(
    date?.from ?? date?.to ?? new Date()
  );

  const [open, setOpen] = React.useState(false);

  return (
    <div className={cn("grid gap-2 ")}>
      <Popover
        open={open}
        onOpenChange={(o) => {
          setOpen(o);

          if (o && (date?.from || date?.to)) {
            setCurrentMonth(date?.from ?? date?.to ?? new Date());
          }
        }}
      >
        <PopoverTrigger asChild>
          <Button
            id="date"
            variant="outline"
            className={cn(
              "w-[300px] justify-start text-left font-normal border-2 rounded-lg border-git-stroke-primary/40 "
              // !date && "text-muted-foreground"
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
          className="w-auto p-4 border-2 border-git-stroke-primary/30"
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
            month={currentMonth}
            onMonthChange={setCurrentMonth}
            selected={date}
            onSelect={handleCalendarSelect}
            numberOfMonths={2}
            disabled={(day) => {
              const today = new Date();

              if (day > today) return true;

              if (date?.from && date?.to) return false;

              return false;
            }}
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
            }}
          />
          {warning && (
            <p className="text-sm text-git-int-primary mt-2 border-accent">
              {warning}
            </p>
          )}
        </PopoverContent>
      </Popover>
    </div>
  );
}
