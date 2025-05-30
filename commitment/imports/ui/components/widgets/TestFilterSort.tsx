import React, { useState } from "react";
import { Card, CardHeader } from "@ui/components/ui/card";
import { CardContent } from "@ui/components/ui/card";
import Filter, { FilterOption, FiltersState, FilterValue } from "../ui/filter";
import { DateRange } from "react-day-picker";

type todo ={
  id:number;
  name:string;
  team:"Alpha"|"Beta"|"Delta"|"Gamma";
  completed:boolean;
  date:Date;
}
const test_docs:todo[] = [
  {
    id: 1,
    name: "Make fish stew",
    team: "Alpha",
    completed: true,
    date: new Date("2025-04-15"),
  },
  {
    id: 2,
    name: "Design Brief",
    team: "Beta",
    completed: false,
    date: new Date("2025-05-01"),
  },
  {
    id: 3,
    name: "Project Proposal",
    team: "Alpha",
    completed: true,
    date: new Date("2025-03-25"),
  },
  {
    id: 4,
    name: "Apple Pie class",
    team: "Delta",
    completed: false,
    date: new Date("2025-05-18"),
  },
  {
    id: 5,
    name: "Sprint Retrospective",
    team: "Gamma",
    completed: false,
    date: new Date("2025-04-05"),
  },
  {
    id: 6,
    name: "User Research Notes",
    team: "Beta",
    completed: true,
    date: new Date("2025-02-28"),
  },
  {
    id: 7,
    name: "Marketing Plan",
    team: "Delta",
    completed: true,
    date: new Date("2025-01-10"),
  },
  {
    id: 8,
    name: "Team Feedback",
    team: "Gamma",
    completed: true,
    date: new Date("2025-05-20"),
  },
  {
    id: 9,
    name: "Budget Review",
    team: "Alpha",
    completed: false,
    date: new Date("2025-03-05"),
  },
  {
    id: 10,
    name: "Wireframe Draft",
    team: "Beta",
    completed: false,
    date: new Date("2025-05-08"),
  },
];

export const TestFilterSort = () => {
  const filterOptions: FilterOption[] = [
    {
      type: "checkbox",
      label: "Is completed",
      filterkey: "completed",
    },
    {
      type: "date",
      label: "Date Created",
      filterkey: "created",
    },

    {
      type: "options",
      label: "Team",
      filterkey: "team",
      options: ["Alpha", "Beta", "Gamma", "Delta"], 
    },
  ];
  
  const [filters, setFilters] = useState<FiltersState>({
    completed: { isUsed: false, value: false },
    created: { isUsed: false, value: { from: undefined, to: undefined } },
    team: { isUsed: false, value: [] },
  });

  const updateFilter = (key: string, value: FilterValue) => {
    setFilters((prev) => ({
      ...prev,
      [key]: {
        isUsed: Array.isArray(value) ? value.length > 0 : !!value, // fix so is accurate/is updated in method
        value,
      },
    }))
    console.log(key,value)
    console.log(filters)
    ;
  };


  // Todo
  const applyFilter = (doc: todo, filters: FiltersState):boolean=>{

    return Object.entries(filters).every(([filterKey, { isUsed, value }]) => {
      if (!isUsed) return true;
  
      switch (filterKey) {
        case "completed":
          return typeof value === "boolean" ? doc.completed === value : true;
  
          // TODO: repair filtering functionality for date range
        case "created":
          if (value && typeof value === "object" && "from" in value && "to" in value) {
            const range = value as DateRange;
            if (range.from && range.to) {
              return doc.date >= range.from && doc.date <= range.to;
            }
          }
          return true;
  
        case "team":
          if (Array.isArray(value)) {
            return value.includes(doc.team);
          }
          return true;
  
        default:
          return true;
      }
    });}


  return (
    <Card>
      <CardHeader>
      <h1> Testing filtering and sorting</h1>
      <div className="flex w-full flex-row justify-end">
      <Filter options={filterOptions} filters={filters} onFilterChange={updateFilter} />
      </div>
      </CardHeader>
      <CardContent className="pt-6  gap-3 content-center">
        <div className="flex  flex-row flex-wrap justify-center gap-3 w-full">
        {test_docs.filter(doc => applyFilter(doc, filters)).map((document) => {
          return (
            <Card className="w-[250px]">
              <CardHeader>
                
                {document.id}

                <h3>{document.name}</h3>
              </CardHeader>
              <CardContent>
              <p>{document.team}</p>
              <p>{document.completed?"Task completed":"Incomplete"}</p>
              <p>{document.date.toString()}</p>
              </CardContent>
            </Card>
          );
        })}
        </div>
      </CardContent>
    </Card>
  );
};
