import React, { useMemo } from "react";
import Chart from "react-apexcharts";
import GraphCard from "./GraphCard";
import { CardHeader, CardTitle } from "../ui/card";
import InfoButton from "../ui/infoButton";
import { HeatMapData } from "/imports/api/types";

// Types
interface HeatmapPoint<X extends string | number = string> {
  x: X;
  rawY: number;
}
interface HeatmapSeries<X extends string | number = string> {
  name: string;
  data: ReadonlyArray<HeatmapPoint<X>>;
}
interface NormalizedPoint<X extends string | number = string> {
  x: X;
  raw: number;
  /** -1 means "none"/zero bucket; otherwise 0 < y <= 1 */
  y: number;
}
interface NormalizedSeries<X extends string | number = string> {
  name: string;
  data: ReadonlyArray<NormalizedPoint<X>>;
}
interface Props {
  data: HeatMapData[];
}

// ------- helpers -------
function getWeekLabel(dateStr: string) {
  const date = new Date(dateStr);
  const year = date.getFullYear();
  const week = Math.ceil(
    ((date.getTime() - new Date(year, 0, 1).getTime()) / 86400000 +
      new Date(year, 0, 1).getDay() +
      1) /
      7
  );
  return `W${week} ${year}`;
}

function processHeatMapData(data: HeatMapData[]) {
  const users = Array.from(new Set(data.map((d) => d.name)));
  const weekLabels = Array.from(
    new Set(data.map((d) => getWeekLabel(d.date)))
  ).sort();

  const series: HeatmapSeries<string>[] = users.map((user) => {
    const weekToCount: Record<string, number> = {};
    data
      .filter((d) => d.name === user)
      .forEach((d) => {
        const week = getWeekLabel(d.date);
        weekToCount[week] = (weekToCount[week] ?? 0) + d.count;
      });
    return {
      name: user,
      data: weekLabels.map((week) => ({
        x: week,
        rawY: weekToCount[week] ?? 0,
      })),
    };
  });

  return { series, weekLabels, users };
}

/** Normalizes each column by its max; zero raw -> y = -1 */
export function normalizeSeriesData<X extends string | number = string>(
  rawSeries: ReadonlyArray<HeatmapSeries<X>>
): NormalizedSeries<X>[] {
  if (rawSeries.length === 0) return [];

  const pointsCount = Math.max(...rawSeries.map((s) => s.data.length));

  const xLabels: (X | number)[] = Array.from(
    { length: pointsCount },
    (_, idx) =>
      rawSeries.find((s) => s.data[idx] !== undefined)?.data[idx].x ?? idx
  );

  const colMax: number[] = Array.from({ length: pointsCount }, (_unused, idx) =>
    rawSeries.reduce<number>((max, series) => {
      const val = series.data[idx]?.rawY ?? 0;
      return val > max ? val : max;
    }, 0)
  );

  return rawSeries.map<NormalizedSeries<X>>((series) => ({
    name: series.name,
    data: Array.from({ length: pointsCount }, (_, idx) => {
      const raw = series.data[idx]?.rawY ?? 0;
      const maxAtIdx = colMax[idx] ?? 0;
      const norm = maxAtIdx === 0 ? 0 : raw / maxAtIdx;
      const y = norm === 0 ? -1 : norm;
      return {
        x: series.data[idx]?.x ?? xLabels[idx],
        raw,
        y,
      };
    }),
  }));
}

/** Sum the raw values in each row (series). */
function computeRowTotals<X extends string | number = string>(
  rawSeries: ReadonlyArray<HeatmapSeries<X>>
): number[] {
  return rawSeries.map((s) =>
    s.data.reduce<number>((acc, p) => acc + p.rawY, 0)
  );
}

function getCssVarValue(varName: string) {
  return getComputedStyle(document.documentElement)
    .getPropertyValue(varName)
    .trim();
}

const levels = [
  { name: "none", color: getCssVarValue("--color-git-bg-elevated") },
  { name: "s1", color: getCssVarValue("--color-git-100") },
  { name: "s2", color: getCssVarValue("--color-git-200") },
  { name: "s3", color: getCssVarValue("--color-git-300") },
  { name: "s4", color: getCssVarValue("--color-git-400") },
  { name: "s5", color: getCssVarValue("--color-git-500") },
  { name: "s6", color: getCssVarValue("--color-git-700") },
  { name: "s7", color: getCssVarValue("--color-git-900") },
];

// ------- component -------
export default function HeatMapTempGraph({ data }: Props): React.ReactElement {
  const { series, weekLabels, users } = useMemo(
    () => processHeatMapData(data),
    [data]
  );
  const chartSeries = useMemo(() => normalizeSeriesData(series), [series]);
  const totals = useMemo(() => computeRowTotals(series), [series]);

  const chartOptions = useMemo(
    () => ({
      chart: {
        height: 350,
        type: "heatmap" as const,
        id: "heatmap-demo",
        toolbar: { show: false },
        parentHeightOffset: 0,
        fontFamily:
          'ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, "Liberation Mono", "Courier New", monospace',
      },
      grid: {
        padding: {
          top: 0,
          right: 0, // room for totals annotations
          bottom: 0,
          left: 28,
        },
      },
      plotOptions: {
        heatmap: {
          shadeIntensity: 0.3,
          radius: 8,
          useFillColorAsStroke: false,
          colorScale: {
            ranges: [
              { from: -1, to: 0, color: levels[0].color, name: "none" },
              { from: 0, to: 0.1429, color: levels[1].color, name: "very low" },
              { from: 0.1429, to: 0.2857, color: levels[2].color, name: "low" },
              {
                from: 0.2857,
                to: 0.4286,
                color: levels[3].color,
                name: "mid-low",
              },
              {
                from: 0.4286,
                to: 0.5714,
                color: levels[4].color,
                name: "medium",
              },
              {
                from: 0.5714,
                to: 0.7143,
                color: levels[5].color,
                name: "mid-high",
              },
              {
                from: 0.7143,
                to: 0.8571,
                color: levels[6].color,
                name: "high",
              },
              {
                from: 0.8571,
                to: 1.0,
                color: levels[7].color,
                name: "extreme",
              },
            ],
          },
        },
      },
      states: {
        hover: { filter: { type: "none" } },
        active: { filter: { type: "none" } },
      },

      xaxis: {
        type: "category" as const,
        tickPlacement: "between",
        labels: {
          trim: false,
          style: { fontSize: "0.875rem", fontWeight: "300" },
        },
      },
      dataLabels: {
        enabled: false,
      },
      stroke: {
        width: 6,
        colors: [getCssVarValue("--color-git-bg-bottom")],
      },

      tooltip: {
        y: {
          formatter(
            value: number,
            {
              seriesIndex,
              dataPointIndex,
              w,
            }: {
              seriesIndex: number;
              dataPointIndex: number;
              w: {
                config: {
                  series: Array<{
                    data: Array<{
                      raw?: number;
                    }>;
                  }>;
                };
              };
            }
          ) {
            const rawY =
              w.config.series[seriesIndex]?.data[dataPointIndex]?.raw;
            const total = totals[seriesIndex];
            return rawY !== undefined
              ? `${rawY} (Total: ${total ?? 0})`
              : String(value);
          },
        },
      },

      legend: {
        show: true,
        position: "bottom",
        horizontalAlign: "right",
        floating: false,
        clusterGroupedSeries: false,
        formatter: () => "",
        markers: { size: 16, shape: "square", radius: 10, strokeWidth: 0 },
        itemMargin: { horizontal: 2, vertical: 0 },
      },
    }),
    [users]
  );

  return (
    <GraphCard className="w-full max-w-[1600px] p-0">
      <CardHeader className="pb-0">
        <CardTitle className="flex text-lg mt-0 font-bold ">
          {"Heat Map - Normalized per Column"}
          <div className="relative -mt-3 ml-2">
            <InfoButton description={"desc"} />
          </div>
        </CardTitle>
      </CardHeader>

      <Chart
        options={chartOptions}
        series={chartSeries}
        type="heatmap"
        width="100%"
        height={800}
      />
    </GraphCard>
  );
}
