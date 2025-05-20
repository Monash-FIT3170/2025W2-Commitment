"use client"

import * as React from "react"
import { addDays, format } from "date-fns"
import { CalendarIcon } from "lucide-react"
import { DateRange } from "react-day-picker"

import { cn } from "@ui/lib/utils"
import { Button } from "@ui/components/ui/button"
import { Calendar } from "@ui/components/ui/calendar"
import {
  Popover,
  PopoverContent,
  PopoverTrigger,
} from "@ui/components/ui/popover"



// todo: allow user to grab info from the date picker

interface RangedDatePicker {
  className?:React.HTMLAttributes<HTMLDivElement>
  setExternalDate: (value:DateRange | undefined)=>void

}



export function DatePickerWithRange({
  className,setExternalDate
}: RangedDatePicker) {

  const [date, setDate] = React.useState<DateRange | undefined>(undefined)

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
        <PopoverContent className="w-auto p-0" align="start">
          <Calendar
            initialFocus
            mode="range"
            defaultMonth={date?.from}
            selected={date}
            onSelect={handleSetDate}
            numberOfMonths={2}
          />
        </PopoverContent>
      </Popover>
    </div>
  )
}
