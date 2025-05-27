import React from "react";
import { Card, CardContent, CardFooter, CardHeader, CardTitle } from "../ui/card";
import { ArrowUpRight } from "lucide-react";
import {  AreaChart, Area} from "recharts";

interface HighlightCardWithGraphProps {
  title: string;
  value: string | number;
  percentageChange?: string | number;
  isPositive?: boolean; // determine arrows direction 
  data?: { value: number }[]; // line data for the graph
}

export const HighlightCardWithGraph: React.FC<HighlightCardWithGraphProps> = ({
  title,
  value,
  percentageChange,
  isPositive = true,
  data = [],
}) => {
  const fillColor = isPositive ? "#59A14F" : "#E15759";
  return (
    <Card  className={`font-mono flex flex-col w-[220px] h-[220px] bg-[#f0f0e8] border border-gray-300 rounded-xl  ${    percentageChange ? "justify-between" : "justify-center" }`}>
      {/* Title & Menu Icon */}
      <CardHeader className="h-[100px]">
        <CardTitle className="flex justify-between items-start text-2xl mt-0 font-normal ">
          {title}
        </CardTitle>
      </CardHeader>
      {/* Value */}
      <CardContent className="text-[26px] font-bold text-3xl ">
        {value.toLocaleString()}
      </CardContent>
      {/* Footer: Change + Graph */}
      {percentageChange ? (
      <CardFooter className="flex justify-between items-center gap-x-4">         
        {/* Percentage Change */}
        <div className="w-[80px] h-[40px]">
   
          <div className="flex items-center gap-1 text-2xl font-semibold leading-[40px]"
              style={{ color:fillColor}}> 
            
            {/* Show arrow pointing up or down */}
            <ArrowUpRight
              size={16}
              style={{ transform: isPositive ? "none" : "rotate(180deg)" }}
            />

            {/* Show the percentage text */}
            <span>{percentageChange}</span>
          </div>

        </div>
      
        {/* Graph */}
        <div className="w-[100px] h-[40px] mr-0">
          <AreaChart width={80} height={40} data={data}>
            <Area
              type="monotone"
              dataKey="value"
              stroke={fillColor}
              fill={fillColor}
              fillOpacity={0.3}
              strokeWidth={2}
            />
          </AreaChart>
        </div>
      </CardFooter>
      ) : null}
    </Card>
  );
};



