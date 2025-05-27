import React from "react";
import InfoButton from "../ui/infoButton";

import {
  CartesianGrid,
  Line,
  LineChart,
  ResponsiveContainer,
  XAxis,
  YAxis,
} from "recharts";
import { CardContent, CardHeader, CardTitle } from "../ui/card";

import {
  ChartConfig,
  ChartContainer,
  ChartTooltip,
  ChartTooltipContent,
} from "../ui/chart";
import GraphCard from "./GraphCard";

// Interface for the contributor data point
interface ContributorDataPoint {
  date: string;
  [contributor: string]: string | number;
}

interface ContributorsLineChart {
  data: ContributorDataPoint[];
  title: string;
  xAxisLabel?: string; // optional x-axis label
  yAxisLabel?: string; // optional y-axis label
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

// dynamically changes the tick margin for the x-axis based on the data length
const tickMarginForXAxis = (dataLength: number) => {
  if (dataLength <= 5) return 6;
  if (dataLength <= 10) return 4;
  return 2;
};


// container with the contributor line graph.
export const ContributorLineGraph: React.FC<ContributorsLineChart> = ({
  data,
  title,
  xAxisLabel,
  yAxisLabel,
}) => {
  // gets the contributors from the data, assuming data is formatted: {date="2023-01-01", contributor1: 1, contributor2: 2}
  const contributors = Object.keys(data[0] || {}).filter(
    (key) => key !== "date"
  );

  // creates a config object for each contributor with a specific color
  const chartConfig: ChartConfig = {};

  // Maps over the contributors and assigns a color to each one
  // staticColorPalette is used for the first 10 contributors, and extendColorPalette is used for the rest
  contributors.forEach((contributor, index) => {
    const color =
      staticColorPalette[index] ??
      extendColorPalette(index - staticColorPalette.length);
    chartConfig[contributor] = {
      label: contributor,
      color,
    } satisfies ChartConfig[string];
  });

  // card with the line graph
  return (
    // <Card  className="flex flex-col w-[475px] h-[400px] bg-[#f0f0e8] border-0 rounded-xl" style={{ backgroundColor: graphBackgroundColour }}>
    <GraphCard>
      {/* Title*/}
      <CardHeader className="h-[100px] flex items-center px-4">
        <CardTitle className="flex justify-between items-start text-lg mt-0 font-bold ">
          {title}
        </CardTitle>
        <CardTitle>
          <div className="relative -mt-2">
            <InfoButton description="Shows contributor performance over time." />
          </div>
        </CardTitle>
      </CardHeader>
      {/* Graph */}
      {/* <CardContent style={{ height: 300, display: "block" }}>            */}
      <CardContent className="flex-grow flex flex-col items-center justify-center pt-2">
        <ChartContainer config={chartConfig} className="flex-grow w-full">
          <ResponsiveContainer width="100%" height="100%">
            <LineChart
              data={data}
              margin={{ top: 12, right: 12, bottom: 20, left: 12 }}
            >
              <CartesianGrid vertical={true} strokeDasharray="3 3" />
              <XAxis
                dataKey="date"
                tickLine={true}
                axisLine={true}
                tickMargin={tickMarginForXAxis(data.length)}
                label={{
                  value: xAxisLabel,
                  position: "insideBottom",
                  offset: -5,
                }}
                tick={{ fontSize: 11 }}
              />
              <YAxis
                tickLine={false}
                axisLine={true}
                tickMargin={2}
                label={{
                  value: yAxisLabel,
                  angle: -90,
                  position: "center",
                  dy: 0,
                  dx: -20,
                  style: { textAnchor: "middle" },
                }}
              />
              <ChartTooltip cursor={false} content={<ChartTooltipContent />} />
              {contributors.map((contributor) => (
                <Line
                  key={contributor}
                  dataKey={contributor}
                  type="monotone"
                  stroke={chartConfig[contributor].color}
                  strokeWidth={2}
                  dot={false}
                />
              ))}
            </LineChart>
          </ResponsiveContainer>
        </ChartContainer>
      </CardContent>
    </GraphCard>
    // </Card>
  );
};
