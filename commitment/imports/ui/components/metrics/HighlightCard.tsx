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
      className={`font-mono text-foreground justify-between flex flex-col w-full min-w-[220px] min-h-[60px] 2xl:min-h-[80px] gap-2 rounded-xl outline-solid outline-2 outline-git-bg-secondary bg-git-bg-bottom`}
    >
      <div className="flex flex-row justify-between items-center h-full p-3">
        {/* Title */}
        <div className="flex-1">
          <CardTitle className="text-lg font-bold text-foreground leading-tight">
            {title}
          </CardTitle>
        </div>
        {/* Value */}
        <div className="text-right">
          <div className="text-2xl font-bold text-foreground">
            {typeof value === 'string' ? value : value.toLocaleString()}
          </div>
        </div>
      </div>
    </Card>
  );
};
