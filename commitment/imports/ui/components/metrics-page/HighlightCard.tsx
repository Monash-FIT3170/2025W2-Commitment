import React from "react";
import { ArrowUpRight } from "lucide-react";
import { AreaChart, Area, ResponsiveContainer } from "recharts";
import {
  Card,
  CardContent,
  CardFooter,
  CardHeader,
  CardTitle,
} from "../ui/card";

interface HighlightCardWithGraphProps {
  title: string;
  value: string | number;
  percentageChange?: string | number;
  isPositive?: boolean; // determine arrows direction
  data?: { value: number }[]; // line data for the graph
}
const graphBackgroundColour = "#E8E8DD";

export const HighlightCardWithGraph: React.FC<HighlightCardWithGraphProps> = ({
  title,
  value,
  percentageChange,
  isPositive = true,
  data = [],
}) => {
  const fillColor = isPositive ? "#59A14F" : "#E15759";
  return (
    <Card
      className={`font-mono text-foreground flex flex-col w-[220px] h-[220px] rounded-xl outline-solid outline-2 outline-git-bg-secondary  bg-git-bg-bottom ${
        percentageChange ? "justify-between" : "justify-center"
      }`}
    >
      {/* Title & Menu Icon */}
      <CardHeader className="pb-0">
        <CardTitle className="flex justify-between items-start md:text-3xl lg:text-3xl xl:text-3xl text-2xl mt-0 font-normal ">
          {title}
        </CardTitle>
      </CardHeader>
      {/* Value */}
      <CardContent className="font-bold md:text-4xl xl:text-4xl text-3xl ">
        {value.toLocaleString()}
      </CardContent>
      {/* Footer: Change + Graph */}
      {percentageChange ? (
        <CardFooter className="flex justify-between items-center gap-x-4">
          {/* Percentage Change */}
          <div className="w-[160px] h-[80px]">
            <div
              className="flex items-center gap-1 text-4xl font-semibold leading-[40px]"
              style={{ color: fillColor }}
            >
              {/* Show arrow pointing up or down */}
              <ArrowUpRight
                size={30}
                style={{ transform: isPositive ? "none" : "rotate(90deg)" }}
              />

              {/* Show the percentage text */}
              <span>{percentageChange}%</span>
            </div>
          </div>

          {/* Graph */}
          <div className="w-full h-[80px]">
            <ResponsiveContainer width="100%" height="100%">
              <AreaChart data={data}>
                <Area
                  type="monotone"
                  dataKey="value"
                  stroke={fillColor}
                  fill={fillColor}
                  fillOpacity={0.3}
                  strokeWidth={2}
                />
              </AreaChart>
            </ResponsiveContainer>
          </div>
        </CardFooter>
      ) : null}
    </Card>
  );
};
