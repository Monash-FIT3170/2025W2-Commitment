"use client"

import * as React from "react"
import { format, subMonths } from "date-fns"
import { CalendarIcon } from "lucide-react"

import { cn } from "@ui/lib/utils"
import { Button } from "@ui/components/ui/button"
import { Calendar } from "@ui/components/ui/calendar"
import {
  Popover,
  PopoverContent,
  PopoverTrigger,
} from "@ui/components/ui/popover"

import { DateRange } from "react-day-picker"

export function DateRangePicker() {
const [date, setDate] = React.useState<DateRange | undefined>(undefined)


  const formattedRange = date?.from
    ? date.to
      ? `${format(date.from, "LLL dd, y")} - ${format(date.to, "LLL dd, y")}`
      : format(date.from, "LLL dd, y")
    : "Pick a date range"

    const today = new Date()
    const previousMonth = subMonths(today, 1)

  return (
    <Popover>
      <PopoverTrigger asChild>
        <Button
          variant={"outline"}
          className={cn(
            "w-[280px] justify-start text-left font-normal",
            !date && "text-muted-foreground"
          )}
        >
          <CalendarIcon className="mr-2 h-4 w-4" />
          {formattedRange}
        </Button>
      </PopoverTrigger>
      <PopoverContent className="w-auto p-0" align="start">
        <Calendar
  initialFocus
  mode="range"
  defaultMonth={previousMonth}
  selected={date}
  onSelect={setDate}
  numberOfMonths={2}
  classNames={{
    day_selected: "bg-[#F1502F] text-white hover:bg-[#F1502F]",
    day_range_middle: "bg-[#F1502F]/30 text-black",
    day_range_start: "rounded-l-md bg-[#F1502F] text-white hover:bg-[#F1502F]",
    day_range_end: "rounded-r-md bg-[#F1502F] text-white hover:bg-[#F1502F]",
  }}
/>


      </PopoverContent>
    </Popover>
  )
}
