import React, { useState } from "react";
import { Card, CardHeader } from "@ui/components/ui/card";
import { CardContent } from "@ui/components/ui/card";
import Filter, { FilterOption, FiltersState, FilterValue } from "../ui/filter";

type doc ={
  id:number;
  name:string;
  team:"Alpha"|"Beta"|"Delta"|"Gamma";
  subscription:boolean;
  date:Date;
}
const test_docs:doc[] = [
  {
    id: 1,
    name: "Invoice #1001",
    team: "Alpha",
    subscription: true,
    date: new Date("2024-04-15"),
  },
  {
    id: 2,
    name: "Design Brief",
    team: "Beta",
    subscription: false,
    date: new Date("2024-05-01"),
  },
  {
    id: 3,
    name: "Project Proposal",
    team: "Alpha",
    subscription: true,
    date: new Date("2024-03-25"),
  },
  {
    id: 4,
    name: "Invoice #1002",
    team: "Delta",
    subscription: false,
    date: new Date("2024-05-18"),
  },
  {
    id: 5,
    name: "Sprint Retrospective",
    team: "Gamma",
    subscription: false,
    date: new Date("2024-04-05"),
  },
  {
    id: 6,
    name: "User Research Notes",
    team: "Beta",
    subscription: true,
    date: new Date("2024-02-28"),
  },
  {
    id: 7,
    name: "Marketing Plan",
    team: "Delta",
    subscription: true,
    date: new Date("2024-01-10"),
  },
  {
    id: 8,
    name: "Team Feedback",
    team: "Gamma",
    subscription: true,
    date: new Date("2024-05-20"),
  },
  {
    id: 9,
    name: "Budget Review",
    team: "Alpha",
    subscription: false,
    date: new Date("2024-03-05"),
  },
  {
    id: 10,
    name: "Wireframe Draft",
    team: "Beta",
    subscription: false,
    date: new Date("2024-05-08"),
  },
];

export const TestFilterSort = () => {
  const filterOptions: FilterOption[] = [
    {
      type: "checkbox",
      label: "Has Subscription",
      filterkey: "subscription",
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
      options: ["Alpha", "Beta", "Gamma", "Delta"], // Adjust based on actual data
    },
  ];
  
  const [filters, setFilters] = useState<FiltersState>({
    subscription: { isUsed: false, value: false },
    created: { isUsed: false, value: { from: undefined, to: undefined } },
    team: { isUsed: false, value: [] },
  });

  const updateFilter = (key: string, value: FilterValue) => {
    setFilters((prev) => ({
      ...prev,
      [key]: {
        isUsed: Array.isArray(value) ? value.length > 0 : !!value,
        value,
      },
    }))
    console.log(key,value)
    console.log(filters)
    ;
  };


  return (
    <Card>
      <CardHeader>
      <h1> Testing filtering and sorting</h1>
      <Filter options={filterOptions} filters={filters} onFilterChange={updateFilter} />

      </CardHeader>
      <CardContent className="pt-6 flex flex-row gap-3 content-center">
        <div className="flex flex-row gap-3 w-3/5">
        {test_docs.map((document,idx) => {
          return (
            <Card className="w-[250px]">
              <CardHeader>
                {document.id}

                <h3>{document.name}</h3>
              </CardHeader>
              <CardContent>
              <p>{document.team}</p>
              <p>{document.subscription}</p>
              </CardContent>
            </Card>
          );
        })}
        </div>
      </CardContent>
    </Card>
  );
};
