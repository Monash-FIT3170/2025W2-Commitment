import {
  DropdownMenu,
  DropdownMenuTrigger,
  DropdownMenuContent,
  DropdownMenuCheckboxItem,
  DropdownMenuSeparator,
} from "@ui/components/ui/dropdown";
import { Label } from "@ui/components/ui/label";
import { Button } from "./button";
import { DateRangePicker } from "./datepicker";
import { Filter as FilterIcon } from "lucide-react";
import { DateRange } from "react-day-picker";
import React from "react";


export type FilterOption =
  | {
      type: "checkbox" | "date";
      label: string;
      filterkey: string;
    }
  | {
      type: "options";
      label: string;
      filterkey: string;
      options: string[];
    };


export type FilterValue = boolean | string[] | DateRange| string;

export type FiltersState = Record<
  string,          //  filterkey
  {
    isUsed: boolean;
    value: FilterValue;
  }
>;

interface FilterProps {
  options: FilterOption[];
  filters: FiltersState;
  onFilterChange: (key: string, value: FilterValue) => void;
}

export default function Filter({ options, filters, onFilterChange }: FilterProps) {
  const getDefaultFilterValue=(opt: FilterOption): FilterValue =>{
    switch (opt.type) {
      case "checkbox":
        return false;
      case "date":
        return { from: undefined, to: undefined } as DateRange;
      case "options":
        return [] as string[];
      default:
        return ""; // fallback if needed
    }
  }
const resetFilters=()=>{
  options.forEach((option)=>{
    const filterState = filters[option.filterkey];

    onFilterChange(option.filterkey,getDefaultFilterValue(option))

  }


)


}

  return (
    <DropdownMenu>
      <DropdownMenuTrigger asChild>
        <Button variant="outline" size="icon">
          <FilterIcon />
        </Button>
      </DropdownMenuTrigger>
      <DropdownMenuContent>
        {options.map((opt, index) => {
          const filterState = filters[opt.filterkey];
          const currentValue = filterState?.value;

          return (
            <div  className="p-2 space-y-2">
              {opt.type === "checkbox" && (
                <DropdownMenuCheckboxItem
                // key={opt.filterkey}
                  checked={Boolean(currentValue)}
                  onCheckedChange={() => {
                    const newVal = !currentValue;
                    onFilterChange(opt.filterkey, newVal);
                  }}
                >
                  {opt.label}
                </DropdownMenuCheckboxItem>
              )}

              {opt.type === "date" && (
                <>
                  <Label>{opt.label}</Label>
                  <DateRangePicker
                    value={currentValue as DateRange}
                    setExternalDate={(value) =>
                      onFilterChange(opt.filterkey, 
                       value || { from: undefined, to: undefined },
                      )}

                    // onChange={(range) => {
                    //   onFilterChange(opt.filterkey, range);
                    // }}
                  />
                </>
              )}

              {opt.type === "options" && (
                <>
                  <Label>{opt.label}</Label>
                  {(opt.options || []).map((option) => {
                    const selectedOptions = (currentValue as string[]) || [];
                    const isChecked = selectedOptions.includes(option);
                    return (
                      <DropdownMenuCheckboxItem
                        // key={option}
                        checked={isChecked}
                        onCheckedChange={() => {
                          const updatedOptions = isChecked
                            ? selectedOptions.filter((v) => v !== option)
                            : [...selectedOptions, option];
                          onFilterChange(opt.filterkey, updatedOptions);
                        }}
                      >
                        {option}
                      </DropdownMenuCheckboxItem>
                    );
                  })}
                </>
              )}


              {index < options.length - 1 && <DropdownMenuSeparator />}
            </div>
          );
        })}

<div className="p-2">
          <Button className="w-full" variant="default" size="sm" onClick={resetFilters}>
            Clear All Filters
          </Button>
        </div>
      </DropdownMenuContent>
    </DropdownMenu>)}
