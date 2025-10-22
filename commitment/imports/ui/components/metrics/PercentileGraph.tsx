import React, { useMemo } from "react";
import {
  ResponsiveContainer,
  ScatterChart,
  Scatter,
  XAxis,
  YAxis,
  Tooltip,
  CartesianGrid,
} from "recharts";
import GraphCard from "./GraphCard";
import { CardHeader, CardTitle } from "@base/card";
import InfoButton from "@base/infoButton";
import { ScalingDistributionResult, ContributorScaledData } from "@api/types";
import { ChartContainer, ChartTooltip, ChartTooltipContent } from "@base/chart";
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from "@base/select";

/** ---------------- Helpers ---------------- */

// Build plot data with stacking per (quartile, roundedPercentile)
function makePlotData(contributors: ContributorScaledData[]) {
  // counters[q][roundedPct] = current stack height (1..n)
  const counters: Record<number, number> = {};
  const data: any[] = [];

  for (const c of contributors) {
    const pct = Number(c.scaledMetric.percentile ?? 0);
    const q = Math.round(pct / 10) * 10; // 0..100 as number

    counters[q] = (counters[q] || 0) + 1; // increment single counter per decile

    data.push({
      x: q, // category x (Q1..Q4 or "0".."100")
      y: counters[q], // stack index so dots pile up
      name: c.contributor.name,
      percentile: pct,
      value: Number(c.scaledMetric.value ?? 0),
    });
  }

  // Sort data so categories are stable (Q1→Q4) and then by percentile

  data.sort((a, b) => Number(a.x) - Number(b.x));

  const maxStack = Math.max(1, ...Object.values(counters));

  return { data, maxStack };
}

// Optional: image “dot” renderer (falls back to a circle)
const AvatarDot = (props: { cx?: any; cy?: any; payload?: any; r?: number; }) => {
  const { cx, cy, payload, r = 20 } = props; // Default r to 20 if undefined
  const url = payload?.avatarUrl;
  const name = payload?.name || "";
  const initials = name.slice(0, 1).toUpperCase();

  // If you prefer a colored circle when no image
  if (!url) {
    return (
      <g>
        <circle
          cx={cx}
          cy={cy}
          r={r}
          fill="#cc2706"
          opacity={0.9}
          stroke="#333"
          strokeWidth={0.9}
        />
        <text
          x={cx}
          y={cy + 8} // offset to vertically center
          textAnchor="middle"
          fontSize={r * 1.2}
          fill="white"
          fontWeight="bold"
          pointerEvents="none"
        >
          {initials}
        </text>
      </g>
    );
  }
  // SVG image; clip to circle by drawing the image inside a <g> with a clipPath
  const clipId = `clip-${payload.name.replace(/\s+/g, "-")}-${cx}-${cy}`;
  return (
    <g>
      <clipPath id={clipId}>
        <circle cx={cx} cy={cy} r={r} />
      </clipPath>
      <circle
        cx={cx}
        cy={cy}
        r={r}
        stroke="#333"
        strokeWidth={0.9}
        fill="white"
        opacity={0.9}
      />
      <image
        href={url}
        x={cx - r}
        y={cy - r}
        width={2 * r}
        height={2 * r}
        preserveAspectRatio="xMidYMid slice"
        clipPath={`url(#${clipId})`}
      />
    </g>
  );
};

interface PercentileGraphProps {
  data: ScalingDistributionResult;
  title?: string;
  setGraphType?: (v: "percentile" | "heatmap") => void;
}

export default function PercentileGraph({
  data,
  title,
  setGraphType,
}: PercentileGraphProps) {
  const { data: plotData, maxStack } = useMemo(
    () => makePlotData(data.contributors),
    [data]
  );

  

  console.table(
  plotData.reduce((m,p)=>((m[p.x]=(m[p.x]??0)+1),m),{} as Record<number,number>)
);

  return (
    <GraphCard className="w-full p-0">
      <CardHeader className="pb-0">
        <CardTitle className="flex justify-between flex-wrap text-xl mt-0 font-bold gap-2">
          <div className="flex gap-2">
            <span className="">{title}</span>
            <div className="relative -mt-3">
              <InfoButton description="Each circle is a contributor. X groups by quartile (or exact percentile), Y is a stack index so overlapping values pile upward." />
            </div>
          </div>
          <Select
            defaultValue="percentile"
            onValueChange={
              setGraphType
                ? (v: "percentile" | "heatmap") => setGraphType(v)
                : undefined
            }
          >
            <SelectTrigger className="w-[180px] bg-git-bg-elevated text-git-foreground font-normal shadow-none">
              <SelectValue />
            </SelectTrigger>
            <SelectContent>
              <SelectItem value="heatmap">Heatmap</SelectItem>
              <SelectItem value="percentile">Percentile</SelectItem>
            </SelectContent>
          </Select>
        </CardTitle>
      </CardHeader>

      <div className="h-[600px]  w-full px-4 pb-4">
        <ChartContainer
          config={{
            x: { label: "Percentile" },
            value: { label: "Percentile" },
          }}
          className="w-full h-full"
        >
          <ScatterChart margin={{ top: 16, right: 24, bottom: 8, left: 8 }}>
            <CartesianGrid strokeDasharray="3 3" horizontal={false} />
            <XAxis
              type="number"
              dataKey="x"
              domain={[0, 100]}
              allowDuplicatedCategory={false}
              ticks={[0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100]}
              label={{
                value: "Percentile",
                position: "insideBottom",
                offset: -6,
              }}
              tick={{ fontSize: 14 }}
            />
            <YAxis
              type="number"
              dataKey="y"
              domain={[0, Math.max(3, maxStack + 1)]}
              hide // stack height is internal; hide for cleaner look
            />
            <Tooltip
              cursor={false}
              content={
                <ChartTooltipContent
                  labelFormatter={(_, payload) => payload?.[0]?.payload?.name}
                  formatter={(val, name, item, _idx, raw) => {
                    if (name === "y")
                      return (
                        <div className="flex w-full items-center justify-between gap-2">
                          <span className="text-muted-foreground">
                            {data.repoDistributions.metric}
                          </span>
                          <span className="font-mono font-medium tabular-nums text-foreground">
                            {item?.payload?.value}
                          </span>
                        </div>
                      );
                    return (
                      <div className="flex w-full items-center justify-between gap-2">
                        <span className="text-muted-foreground">
                          Percentile
                        </span>
                        <span className="font-mono font-medium tabular-nums text-foreground">
                          {val}
                        </span>
                      </div>
                    );
                  }}
                />
              }
            />

            <Scatter data={plotData} shape={<AvatarDot r={23} />} />
          </ScatterChart>
        </ChartContainer>
      </div>
    </GraphCard>
  );
}
