import React, { useState } from "react";
import { Card, CardHeader } from "@ui/components/ui/card";
import { CardContent } from "@ui/components/ui/card";
import Filter, { FilterOption } from "../ui/filter";

const test_docs = [
  {
    id: 1,
    name: "Invoice #1001",
    category: "Finance",
    team: "Alpha",
    subscription: true,
    date: new Date("2024-04-15"),
  },
  {
    id: 2,
    name: "Design Brief",
    category: "Design",
    team: "Beta",
    subscription: false,
    date: new Date("2024-05-01"),
  },
  {
    id: 3,
    name: "Project Proposal",
    category: "Planning",
    team: "Alpha",
    subscription: true,
    date: new Date("2024-03-25"),
  },
  {
    id: 4,
    name: "Invoice #1002",
    category: "Finance",
    team: "Delta",
    subscription: false,
    date: new Date("2024-05-18"),
  },
  {
    id: 5,
    name: "Sprint Retrospective",
    category: "Agile",
    team: "Gamma",
    subscription: false,
    date: new Date("2024-04-05"),
  },
  {
    id: 6,
    name: "User Research Notes",
    category: "Research",
    team: "Beta",
    subscription: true,
    date: new Date("2024-02-28"),
  },
  {
    id: 7,
    name: "Marketing Plan",
    category: "Marketing",
    team: "Delta",
    subscription: true,
    date: new Date("2024-01-10"),
  },
  {
    id: 8,
    name: "Team Feedback",
    category: "Agile",
    team: "Gamma",
    subscription: true,
    date: new Date("2024-05-20"),
  },
  {
    id: 9,
    name: "Budget Review",
    category: "Finance",
    team: "Alpha",
    subscription: false,
    date: new Date("2024-03-05"),
  },
  {
    id: 10,
    name: "Wireframe Draft",
    category: "Design",
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
      filterKey: "subscription",
    },
    {
      type: "date",
      label: "Created Date",
      filterKey: "date",
    },
    {
      type: "options",
      label: "Category",
      filterKey: "category",
      options: [
        "Finance",
        "Design",
        "Planning",
        "Agile",
        "Research",
        "Marketing",
      ],
    },
    {
      type: "options",
      label: "Team",
      filterKey: "team",
      options: ["Alpha", "Beta", "Gamma", "Delta"],
    },
  ];

  // filter will be type filterkey, then the associated value
  const [filters, setFilters] = useState(null);

  return (
    <Card>
      <CardHeader>
      <h1> Testing filtering and sorting</h1>
        <Filter />

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
              <p>{document.category}</p>
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
