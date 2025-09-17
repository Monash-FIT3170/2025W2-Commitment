import React from "react";
import { PieChart, Pie, Cell, Tooltip, Legend } from "recharts";
import { TrendingUp } from "lucide-react";
import {
  Card,
  CardContent,
  CardFooter,
  CardHeader,
  CardTitle,
} from "@ui/components/ui/card";
import InfoButton from "../ui/infoButton";
import GraphCard from "./GraphCard";
import { ChartContainer, ChartTooltip, ChartTooltipContent } from "../ui/chart";

export interface ChartEntry {
  user: string;
  contributions: number;
}

interface Props {
  data: ChartEntry[];
  title: string;
}

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
  "Commit distribution by contributor — each slice shows a contributor's share of total commits.";

// Main Pie Chart
export function ContributionPieChart({ data, title }: Props) {
  const coloredData = data.map((entry, index) => ({
    ...entry,
    title,
    fill: staticColorPalette[index] ?? extendColorPalette(index),
  }));
  if (!data || data.length === 0) {
    return (
      <GraphCard className="w-full max-w-full h-[500px] flex flex-col basis-1/3">
        <CardHeader className="pb-0">
          <CardTitle className="flex text-xl mt-0 font-bold ">
            {title}
            <div className="relative -mt-3 ml-2">
              <InfoButton description={pieChartDescription} />
            </div>
          </CardTitle>
        </CardHeader>
        <CardContent className="grow flex flex-col items-center justify-center pt-2">
          <div className="text-gray-500 text-center py-8">
            No contribution data available.
          </div>
        </CardContent>
      </GraphCard>
    );
  }

  return (
    <GraphCard className="w-full max-w-full h-[500px] flex flex-col basis-1/3">
      <CardHeader className="pb-0">
        <div className="flex items-center w-full gap-2">
          <h2 className="text-xl font-bold"> {title}</h2>
          <div className="relative -mt-2">
            <InfoButton description={pieChartDescription} />
          </div>
        </div>
      </CardHeader>

      {coloredData.length === 0 ? (
        <CardContent className="p-4 text-gray-500">
          Please select an End Date in the Date Range
        </CardContent>
      ) : (
        <CardContent className="flex flex-col items-center gap-4">
          {/* Legend */}
          <div className="w-full overflow-hidden">
            <ul className="flex flex-wrap justify-center gap-x-4 gap-y-1 text-xs max-w-full">
              {coloredData.map((entry) => (
                <li key={entry.user} className="flex items-center gap-1">
                  <span
                    className="inline-block h-3 w-3 rounded-sm"
                    style={{ backgroundColor: entry.fill }}
                  />
                  <span className="whitespace-nowrap">{entry.user}</span>
                </li>
              ))}
            </ul>
          </div>

          {/* Pie Chart */}
          <ChartContainer
            config={{
              contributions: {
                label: "Contributions",
              },
              user: {
                label: "User",
              },
            }}
            className="min-h-[300px] w-full max-w-[300px]"
          >
            <PieChart>
              <Pie
                data={coloredData}
                dataKey="contributions"
                nameKey="user"
                cx="50%"
                cy="50%"
                outerRadius={100}
                innerRadius={0}
                stroke="none"
                strokeWidth={0}
                isAnimationActive
                animationDuration={800}
                labelLine={false}
              >
                {coloredData.map((entry) => (
                  <Cell
                    key={entry.user}
                    fill={entry.fill}
                    stroke="none"
                    strokeWidth={0}
                  />
                ))}
              </Pie>
              <ChartTooltip cursor={false} content={<ChartTooltipContent />} />
            </PieChart>
          </ChartContainer>
        </CardContent>
      )}
    </GraphCard>
  );
}
