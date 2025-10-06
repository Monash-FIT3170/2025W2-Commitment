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

/** ---------------- Example data ----------------
 * Matches your shape: { contributorName, scaledMetric: { metric, value, percentile } }
 * Added avatarUrl just to demo image dots; omit if you don’t want images.
 */
const exampleContributors = [
  {
    contributorName: "Amy Tjea",
    avatarUrl: "https://github.com/AmyTjea.png",
    scaledMetric: { metric: "Total No. Commits", value: 5, percentile: 66.67 },
  },
  {
    contributorName: "Milni Abeysekara",
    avatarUrl: "https://github.com/milnia4.png",
    scaledMetric: { metric: "Total No. Commits", value: 6, percentile: 83.33 },
  },
  {
    contributorName: "Nicholas Bisset",
    avatarUrl: "https://github.com/Densetsu152637.png",
    scaledMetric: { metric: "Total No. Commits", value: 0, percentile: 0 },
  },
  {
    contributorName: "Ishrat Kaur",
    avatarUrl: "https://github.com/QodeWiz.png",
    scaledMetric: { metric: "Total No. Commits", value: 3, percentile: 33.33 },
  },
  {
    contributorName: "Muhammad Yoonus Nazeem",
    avatarUrl: "https://github.com/YoonusNazz.png",
    scaledMetric: { metric: "Total No. Commits", value: 4, percentile: 50 },
  },
  {
    contributorName: "Janidu Hathurusinghe",
    avatarUrl: "",
    scaledMetric: { metric: "Total No. Commits", value: 0, percentile: 0 },
  },

  // Add a few more to demonstrate stacking within the same bins:
  {
    contributorName: "A. Lee",
    avatarUrl: "",
    scaledMetric: { metric: "Total No. Commits", value: 1, percentile: 10 },
  },
  {
    contributorName: "B. Chen",
    avatarUrl: "",
    scaledMetric: { metric: "Total No. Commits", value: 2, percentile: 33.33 },
  },
  {
    contributorName: "C. Wu",
    avatarUrl: "",
    scaledMetric: { metric: "Total No. Commits", value: 1, percentile: 33.33 },
  },
  {
    contributorName: "D. Park",
    avatarUrl: "",
    scaledMetric: { metric: "Total No. Commits", value: 7, percentile: 90 },
  },
  {
    contributorName: "E. Kim",
    avatarUrl: "",
    scaledMetric: { metric: "Total No. Commits", value: 7, percentile: 90 },
  },
  {
    contributorName: "F. Roy",
    avatarUrl: "",
    scaledMetric: { metric: "Total No. Commits", value: 7, percentile: 90 },
  },
];

/** ---------------- Helpers ---------------- */

// Build plot data with stacking per (quartile, roundedPercentile)
function makePlotData(contributors: ContributorScaledData[]) {
  // counters[q][roundedPct] = current stack height (1..n)
  const counters: Record<string, Record<number, number>> = {};
  const data: any[] = [];

  for (const c of contributors) {
    const pct = Number(c.scaledMetric.percentile ?? 0);
    const q = String(Math.round(pct)); // x bucket
    const sub = Math.round(pct); // sub-bucket used for stacking within a quartile

    counters[q] ||= {};
    counters[q][sub] = (counters[q][sub] || 0) + 0.5;

    data.push({
      x: q, // category x (Q1..Q4 or "0".."100")
      y: counters[q][sub], // stack index so dots pile up
      name: c.contributor.name,
      percentile: pct,
      value: Number(c.scaledMetric.value ?? 0),
    });
  }

  // Sort data so categories are stable (Q1→Q4) and then by percentile

  data.sort((a, b) => Number(a.x) - Number(b.x));

  const maxStack = Math.max(
    1,
    ...Object.values(counters).map((m) => Math.max(0, ...Object.values(m)))
  );

  return { data, maxStack };
}

// Optional: image “dot” renderer (falls back to a circle)
const AvatarDot = (props) => {
  const { cx, cy, payload, r = 10 } = props;
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
}

export default function PercentileGraph({ data, title }: PercentileGraphProps) {
  const { data: plotData, maxStack } = useMemo(
    () => makePlotData(data.contributors),
    [data]
  );

  console.log(data);
  return (
    <GraphCard className="w-full p-0">
      <CardHeader className="pb-0">
        <CardTitle className="flex justify-between flex-wrap text-xl mt-0 font-bold gap-2">
          <div className="flex gap-2">
            <span className="whitespace-nowrap">{title}</span>
            <div className="relative -mt-3">
              <InfoButton description="Each circle is a contributor. X groups by quartile (or exact percentile), Y is a stack index so overlapping values pile upward." />
            </div>
          </div>
        </CardTitle>
      </CardHeader>

      <div className="h-[380px]  w-full px-4 pb-4">
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
              allowDuplicatedCategory={false}
              ticks={undefined}
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
                          <span className="text-muted-foreground">{data.repoDistributions.metric}</span>
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

            <Scatter data={plotData} shape={<AvatarDot r={20} />} />
          </ScatterChart>
        </ChartContainer>
      </div>
    </GraphCard>
  );
}
