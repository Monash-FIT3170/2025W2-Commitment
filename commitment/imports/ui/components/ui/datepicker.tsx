"use client"

import * as React from "react"
import {  format, subMonths } from "date-fns"
import { CalendarIcon } from "lucide-react"
import { DateRange } from "react-day-picker"

import { cn } from "@ui/lib/utils"
import { Button } from "../ui/button"
import { Calendar } from "../ui/calendar"
import {
  Popover,
  PopoverContent,
  PopoverTrigger,
} from "../ui/popover"



// todo: allow user to grab info from the date picker

interface RangedDatePicker {
  className?:React.HTMLAttributes<HTMLDivElement>
  value:DateRange | undefined; // external state
  setExternalDate: (value:DateRange | undefined)=>void

}



export function DateRangePicker({
  className,value,setExternalDate
}: RangedDatePicker) {

  const [date, setDate] = React.useState<DateRange | undefined>(undefined)


  const formattedRange = value?.from
    ? value.to
      ? `${format(value.from, "LLL dd, y")} - ${format(value.to, "LLL dd, y")}`
      : format(value.from, "LLL dd, y")
    : "Pick a date range"

    const today = new Date()
    const previousMonth = subMonths(today, 1)
  const handleSetDate =(newDates:DateRange | undefined)=>{
    setDate(newDates);
    setExternalDate(newDates);

  }

  return (
    <div className={cn("grid gap-2", className)}>
      <Popover>
        <PopoverTrigger asChild>
          <Button
            id="date"
            variant={"outline"}
            className={cn(
              "w-[300px] justify-start text-left font-normal",
              !date && "text-muted-foreground"
            )}
          >
            <CalendarIcon />
            
            {formattedRange }
          </Button>
        </PopoverTrigger>
        <PopoverContent className="w-auto p-0" align="start">
        <Calendar
            initialFocus
            mode="range"
            defaultMonth={previousMonth}
            selected={date}
            onSelect={handleSetDate}
            numberOfMonths={2}
            classNames={{
            day_selected: "bg-[#F1502F] text-white hover:bg-[#F1502F]",
            day_range_middle: "bg-[#F1502F]/30 text-black",
            day_range_start: "rounded-l-md bg-[#F1502F] text-white hover:bg-[#F1502F]",
            day_range_end: "rounded-r-md bg-[#F1502F] text-white hover:bg-[#F1502F]",}}/>

        </PopoverContent>
      </Popover>
    </div>
  )
}
