"use client";

import React from "react";
import {
  RadialBarChart,
  RadialBar,
  PolarAngleAxis,
} from "recharts";

interface ScalingRadialChartProps {
  value: number; // 0 to 1
}

export function ScalingRadialChart({ value }: ScalingRadialChartProps) {
  const percentage = Math.max(0, Math.min(1, value)); // clamp to [0, 1]
  const chartData = [
    {
      name: "scale",
      value: percentage * 100,
      fill: "#3B82F6",
    },
  ];

  return (
    <div className="w-16 h-16"> {/* 64px container */}
      <RadialBarChart
        width={64}
        height={64}
        cx="50%"
        cy="50%"
        innerRadius="80%"
        outerRadius="100%"
        barSize={8}
        data={chartData}
        startAngle={90}
        endAngle={-270}
      >
        <PolarAngleAxis
          type="number"
          domain={[0, 100]}
          angleAxisId={0}
          tick={false}
        />
        <RadialBar
          background={{ fill: "transparent" }}
          clockWise
          minAngle={0}
          dataKey="value"
          angleAxisId={0}
          cornerRadius={5}
        />
        <text
          x="50%"
          y="50%"
          textAnchor="middle"
          dominantBaseline="middle"
          style={{
            fill: "#ffffff",
            fontSize: "0.7rem",
            fontWeight: "bold",
            
          }}
        >
          {(value * 100).toFixed(0)}%
        </text>
      </RadialBarChart>
    </div>
  );
}


