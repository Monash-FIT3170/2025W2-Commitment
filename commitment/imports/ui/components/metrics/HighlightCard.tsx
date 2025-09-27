import React from "react";
import { ArrowUpRight } from "lucide-react";
import { AreaChart, Area, ResponsiveContainer } from "recharts";
import {
  Card,
  CardContent,
  CardFooter,
  CardHeader,
  CardTitle,
} from "@base/card";

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
      className={`font-mono text-foreground justify-between flex flex-col w-full min-w-[220px] min-h-[168px] gap-2 rounded-xl outline-solid outline-2 outline-git-bg-secondary  bg-git-bg-bottom `}
    >
      <div className="flex flex-row justify-between h-full pb-3">
        {/* Title & Menu Icon */}
        <CardHeader className="pb-0">
          <CardTitle className="flex justify-between items-start font-bold text-2xl mt-0 ">
            {title}
          </CardTitle>
        </CardHeader>
        {/* Value */}
        <CardContent className="font-bold  text-3xl flex items-end pb-0">
          {value.toLocaleString()}
        </CardContent>
      </div>
    </Card>
  );
};
