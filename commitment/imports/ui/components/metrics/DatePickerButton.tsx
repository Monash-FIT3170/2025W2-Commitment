import * as React from "react";
import { format, subMonths } from "date-fns";
import { CalendarIcon } from "lucide-react";

import { cn } from "@ui/lib/utils";
import { Button } from "@base/button";
import { Calendar } from "@base/calendar";
import {
  Popover,
  PopoverContent,
  PopoverTrigger,
} from "@base/popover";

import { DateRange } from "react-day-picker";

type Props = {
  onChange?: (range: DateRange | undefined) => void;
  defaultValue?: DateRange;
};

export function DateRangePicker({ onChange, defaultValue }: Props) {
  const [date, setDate] = React.useState<DateRange | undefined>(defaultValue);

  React.useEffect(() => {
    if (onChange) {
      onChange(date);
    }
  }, [date, onChange]);

  const formattedRange = date?.from
    ? date.to
      ? `${format(date.from, "LLL dd, y")} - ${format(date.to, "LLL dd, y")}`
      : format(date.from, "LLL dd, y")
    : "Pick a date range";

  const today = new Date();
  const previousMonth = subMonths(today, 1);

  return (
    <Popover>
      <PopoverTrigger asChild>
        <Button
          variant="outline"
          className={cn(
            "w-[300px] justify-start text-left font-normal border-2 rounded-lg",
            !date && "text-muted-foreground"
          )}
          style={{ borderColor: "#35353140" }}
        >
          <CalendarIcon className="mr-2 h-4 w-4" />
          {formattedRange}
        </Button>
      </PopoverTrigger>
      <PopoverContent
        className="w-auto p-0 border-2"
        style={{ borderColor: "#252522" }}
        align="start"
      >
        <Calendar
          initialFocus
          mode="range"
          disabled={(date) => date > new Date()}
          defaultMonth={previousMonth}
          selected={date}
          onSelect={setDate}
          numberOfMonths={2}
          classNames={{
            day_selected: "bg-[#F1502F] text-white hover:bg-[#F1502F]",
            day_range_middle: "bg-[#F1502F]/30 text-black",
            day_range_start:
              "rounded-l-md bg-[#F1502F] text-white hover:bg-[#F1502F]",
            day_range_end:
              "rounded-r-md bg-[#F1502F] text-white hover:bg-[#F1502F]",
          }}
        />
      </PopoverContent>
    </Popover>
  );
}
