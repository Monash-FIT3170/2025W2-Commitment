import React, { useMemo } from "react";
import Chart from "react-apexcharts";
import GraphCard from "./GraphCard";
import { CardHeader, CardTitle } from "../ui/card";
import InfoButton from "../ui/infoButton";
import { HeatMapData } from "/imports/api/types";
import { ModeToggle } from "./ModeToggle";

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
  title?: string;
}

type Mode = "week" | "week-fill" | "month";

// ------- helpers -------
function startOfDay(d: Date) {
  const x = new Date(d);
  x.setHours(0, 0, 0, 0);
  return x;
}

function alignToMonday(d: Date) {
  const day = d.getDay(); // 0=Sun, 1=Mon
  const diffToMonday = (day === 0 ? -6 : 1) - day;
  const monday = new Date(d);
  monday.setDate(d.getDate() + diffToMonday);
  return startOfDay(monday);
}

function buildContinuousWeekCategories(data: HeatMapData[]): string[] {
  if (data.length === 0) return [];

  const dates = data.map((d) => startOfDay(new Date(d.date)));
  const minDate = new Date(Math.min(...dates.map((d) => d.getTime())));
  const maxDate = new Date(Math.max(...dates.map((d) => d.getTime())));

  const firstMonday = alignToMonday(minDate);
  const lastMonday = alignToMonday(maxDate);

  const categories: string[] = [];
  for (
    let cur = new Date(firstMonday);
    cur <= lastMonday;
    cur.setDate(cur.getDate() + 7)
  ) {
    // Reuse your existing label logic
    categories.push(getWeekLabel(cur.toISOString()));
  }
  return categories;
}

function getWeekLabel(dateStr: string) {
  const date = new Date(dateStr);

  // Align to Monday as start of week
  const day = date.getDay(); // 0=Sun, 1=Mon
  const diffToMonday = (day === 0 ? -6 : 1) - day;
  const monday = new Date(date);
  monday.setDate(date.getDate() + diffToMonday);

  const sunday = new Date(monday);
  sunday.setDate(monday.getDate() + 6);

  // Example: "12–18 Oct"
  const fmt = new Intl.DateTimeFormat("en-GB", {
    day: "numeric",
    month: "short",
  });

  return `${fmt.format(monday)} - ${fmt.format(sunday)}`;
}

function getMonthLabel(dateStr: string) {
  const d = new Date(dateStr);
  return new Intl.DateTimeFormat("en-GB", {
    month: "short",
    year: "numeric",
  }).format(new Date(d.getFullYear(), d.getMonth(), 1)); // e.g., "Aug 2025"
}

function processHeatMapData(data: HeatMapData[], mode: Mode) {
  const users = Array.from(new Set(data.map((d) => d.name)));

  // key -> firstDate for sorting (month 1st; week uses label's Monday)
  const keyToFirstDay = new Map<string, Date>();
  const makeKeyAndFirstDate = (dateStr: string) => {
    const d = new Date(dateStr);
    if (mode === "week" || mode === "week-fill") {
      const monday = alignToMonday(d);
      return { key: getWeekLabel(dateStr), first: monday };
    } else {
      const first = new Date(d.getFullYear(), d.getMonth(), 1);
      first.setHours(0, 0, 0, 0);
      return { key: getMonthLabel(dateStr), first };
    }
  };

  for (const r of data) {
    const { key, first } = makeKeyAndFirstDate(r.date);
    if (!keyToFirstDay.has(key)) keyToFirstDay.set(key, first);
  }

  // ---- categories per mode ----
  const categories =
    mode === "week-fill"
      ? buildContinuousWeekCategories(data) // padded, continuous weeks
      : mode === "week"
      ? Array.from(new Set(data.map((d) => getWeekLabel(d.date)))).sort(
          (a, b) => {
            const aT = keyToFirstDay.get(a)?.getTime() ?? 0;
            const bT = keyToFirstDay.get(b)?.getTime() ?? 0;
            return aT - bT;
          }
        ) // sparse weeks as-is
      : Array.from(new Set(data.map((d) => getMonthLabel(d.date)))).sort(
          (a, b) => {
            const aT = keyToFirstDay.get(a)?.getTime() ?? 0;
            const bT = keyToFirstDay.get(b)?.getTime() ?? 0;
            return aT - bT;
          }
        );

  // aggregate counts per (user, category)
  const series: HeatmapSeries<string>[] = users.map((user) => {
    const keyToCount: Record<string, number> = {};
    data
      .filter((d) => d.name === user)
      .forEach((d) => {
        const key =
          mode === "month" ? getMonthLabel(d.date) : getWeekLabel(d.date);
        keyToCount[key] = (keyToCount[key] ?? 0) + d.count;
      });

    // project onto chosen categories; missing weeks => 0 (pads in week-fill)
    return {
      name: user,
      data: categories.map((k) => ({ x: k, rawY: keyToCount[k] ?? 0 })),
    };
  });

  return { series, categories, users };
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
export default function HeatMapGraph({
  data,
  title,
}: Props): React.ReactElement {
  const [mode, setMode] = React.useState<Mode>("week");
  const { series, categories, users } = useMemo(
    () => processHeatMapData(data, mode),
    [data, mode]
  );

  // Compute totals and sort series by total descending
  const sortedSeries = useMemo(() => {
    const totals = computeRowTotals(series);
    return [...series]
      .map((s, idx) => ({ s, total: totals[idx] }))
      .sort((a, b) => a.total - b.total)
      .map(({ s }) => s);
  }, [series]);

  const chartSeries = useMemo(
    () => normalizeSeriesData(sortedSeries),
    [sortedSeries]
  );
  const totals = useMemo(() => computeRowTotals(sortedSeries), [sortedSeries]);

  const rowHeight = 50;
  const minHeight = 200;
  const maxHeight = 800;
  const dynamicHeight = Math.max(
    minHeight,
    Math.min(maxHeight, users.length * rowHeight)
  );

  const chartOptions = useMemo(
    () => ({
      chart: {
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
      yaxis: {
        labels: {
          style: {
            fontSize: "14px",
            fontWeight: 500,
          },
        },
      },
      xaxis: {
        categories: categories,
        type: "category" as const,
        tickPlacement: "between",
        labels: {
          trim: false,
          style: { fontSize: "0.875rem", fontWeight: "300" },
          formatter: (label: string) => {
            // If label has a dash, only keep the part before it
            const dashIndex = label.indexOf(" -");
            return dashIndex !== -1 ? label.substring(0, dashIndex) : label;
          },
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
        x: {
          formatter: (label: string) => label,
        },
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
    [categories, users, mode, totals]
  );

  return (
    <GraphCard className="w-full max-w-[1600px] p-0">
      <CardHeader className="pb-0">
        <CardTitle className="flex justify-between text-lg mt-0 font-bold ">
          <div className="flex ">
            {title ?? "Contributions Heatmap"}
            <div className="relative -mt-3 ml-2">
              <InfoButton description="Each cell represents a user's contributions during a specific time period. The color intensity reflects how close their activity is to the highest contribution made by any user in that period" />
            </div>
          </div>
          <ModeToggle value={mode} onChange={(v: Mode) => setMode(v)} />
        </CardTitle>
      </CardHeader>

      <Chart
        options={chartOptions}
        series={chartSeries}
        type="heatmap"
        width="100%"
        height={dynamicHeight}
      />
    </GraphCard>
  );
}
