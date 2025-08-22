"use client";

import React from "react";
import { PieChart, Pie, Cell, Tooltip } from "recharts";

import { TrendingUp } from "lucide-react";

import {
  Card,
  CardContent,
  CardFooter,
  CardHeader,
} from "@ui/components/ui/card";
import InfoButton from "../ui/infoButton";

export interface ChartEntry {
  user: string;
  contributions: number;
  fill: string;
}

interface Props {
  data: ChartEntry[];
}

// CURRENT COLOUR PALLETTE - ASK PMs TO help

const graphBackgroundColour = "#E8E8DD";

const staticColorPalette = [
  "#4E79A7",
  "#F28E2B",
  "#59A14F",
  "#E15759",
  "#76B7B2",
  "#EDC948",
  "#B07AA1",
  "#FF9DA7",
  "#9C755F",
  "#BAB0AC",
  "#D37295",
];

const extendColorPalette = (index: number): string => {
  const hue = (index * 137.508) % 360;
  return `hsl(${hue}, 70%, 55%)`;
};

const pieChartDescription =
  "Who's got the largest slice? Definitely committed!";

// For pop up
const CustomTooltip = ({
  active,
  payload,
}: {
  active?: boolean;
  payload?: any[];
}) => {
  if (!active || !payload || !payload.length) return null;

  const { user, contributions, fill } = payload[0].payload;

  return (
    <div className="rounded-md border-2 border-black bg-white px-3 py-2 text-sm shadow-md text-muted-foreground ">
      <div className="flex items-center gap-2 font-semibold">
        <span
          className="inline-block h-3 w-3 rounded-sm"
          style={{ backgroundColor: fill }}
        />
        {user}
      </div>
      <div>{contributions} contributions</div>
    </div>
  );
};

// Main Pie Chart
export function ContributionPieChart({ data }: Props) {
  const coloredData = data.map((entry, index) => ({
    ...entry,
    fill:
      index < staticColorPalette.length
        ? staticColorPalette[index]
        : extendColorPalette(index - staticColorPalette.length),
  }));
  return (
    <Card
      className="flex flex-col max-w-md mx-auto shadow-none ring-0 border-0 outline-hidden"
      style={{ backgroundColor: graphBackgroundColour }}
    >
      <CardHeader className="items-center pb-0">
        <div className="flex items-center space-x-2 w-4/5">
          <h2 className="text-lg font-bold text-gray-800">{"Pie Chart"}</h2>

          {/* Special margin for the infoButton to get it centred */}
          <div className="relative -mt-2">
            <InfoButton description={pieChartDescription} />
          </div>
        </div>

        {/* <CardDescription>Last 6 months</CardDescription> */}
      </CardHeader>
      
       {coloredData.length === 0 ? (
      <CardContent className="p-4 text-gray-500">Please select an End Date in the Date Range</CardContent>
    ) :(
      <>
        <CardContent className="flex flex-col items-center gap-4">
          {/* Pie */}
          <PieChart width={300} height={300}>
            <Pie
              data={coloredData}
              dataKey="contributions"
              nameKey="user"
              cx="50%"
              cy="50%"
              outerRadius={110}
              stroke="none"
              isAnimationActive={true}
              animationDuration={800}
              labelLine={false}
            >
              {coloredData.map((entry, i) => (
                <Cell key={`cell-${i}`} fill={entry.fill} stroke="none" />
              ))}
            </Pie>
            <Tooltip
              content={<CustomTooltip />}
              wrapperStyle={{ outline: "none" }}
            />
          </PieChart>

          {/* Legend */}
          <div className="w-full flex justify-center">
            <ul className="inline-flex flex-wrap justify-center gap-x-4 gap-y-1 text-sm">
              {data.map((entry, i) => (
                <li key={`legend-${i}`} className="flex items-center gap-1">
                  <span
                    className="inline-block h-3 w-3 rounded-sm"
                    style={{ backgroundColor: entry.fill }}
                  />
                  <span className="truncate">{entry.user}</span>
                </li>
              ))}
            </ul>
          </div>
        </CardContent>

        <CardFooter className="flex flex-col gap-1 text-sm text-muted-foreground">
          <div className="flex items-center gap-2 font-medium leading-none">
            Trending up by 5.2 % this month
            <TrendingUp className="h-4 w-4 text-green-500" />
          </div>
          <p className="leading-none">
            Showing total contributions for the last 6 months
          </p>
        </CardFooter>
      </>
      )}
    </Card>
  );
}
