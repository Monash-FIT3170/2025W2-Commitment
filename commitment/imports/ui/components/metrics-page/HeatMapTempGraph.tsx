import React, { useMemo } from "react";
import Chart from "react-apexcharts";
import GraphCard from "./GraphCard";
import { CardHeader, CardTitle } from "../ui/card";
import InfoButton from "../ui/infoButton";

// Generate raw data
function generateRawData(
  seriesCount: number,
  pointsCount: number,
  min: number,
  max: number
) {
  return Array.from({ length: seriesCount }, (_, sIdx) => ({
    name: `Metric${sIdx + 1}`,
    data: Array.from({ length: pointsCount }, (_, pIdx) => ({
      x: `W${pIdx + 1}`,
      rawY: Math.floor(Math.random() * (max - min + 1)) + min,
    })),
  }));
}

function normalizeSeriesData(rawSeries) {
  const pointsCount = rawSeries[0].data.length;
  const colMax = Array(pointsCount).fill(0);
  rawSeries.forEach((series) => {
    series.data.forEach((point, idx) => {
      if (point.rawY > colMax[idx]) colMax[idx] = point.rawY;
    });
  });
  return rawSeries.map((series) => ({
    name: series.name,
    data: series.data.map((point, idx) => {
      const norm = colMax[idx] === 0 ? 0 : point.rawY / colMax[idx];
      // Give Y a special value for zero to map to "none" color
      const y = norm === 0 ? -1 : norm;
      return { x: point.x, raw: point.rawY, y };
    }),
  }));
}

export default function HeatMapTempGraph(): React.ReactElement {
  const rawSeries = useMemo(() => generateRawData(9, 12, 0, 90), []);
  const chartSeries = useMemo(
    () => normalizeSeriesData(rawSeries),
    [rawSeries]
  );

  function getCssVarValue(varName: string) {
    return getComputedStyle(document.documentElement)
      .getPropertyValue(varName)
      .trim();
  }
  // Use Tailwind CSS custom colors via CSS variables
  const levels = [
    { name: "none", color: getCssVarValue("--color-git-bg-elevated") }, // zero cells
    { name: "s1", color: getCssVarValue("--color-git-100") },
    { name: "s2", color: getCssVarValue("--color-git-200") },
    { name: "s3", color: getCssVarValue("--color-git-300") },
    { name: "s4", color: getCssVarValue("--color-git-400") },
    { name: "s5", color: getCssVarValue("--color-git-500") },
    { name: "s6", color: getCssVarValue("--color-git-700") },
    { name: "s7", color: getCssVarValue("--color-git-900") },
  ];

  const chartOptions = useMemo(
    () => ({
      chart: {
        height: 350,
        type: "heatmap" as const,
        id: "heatmap-demo",
        toolbar: {
          show: false,
        },
        parentHeightOffset: 0,
        fontFamily:
          'ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, "Liberation Mono", "Courier New", monospace',
      },
      grid: {
        padding: {
          top: -20,
          right: 0,
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
        hover: {
          filter: {
            type: "none",
          },
        },
        active: {
          filter: {
            type: "none",
          },
        },
      },
      dataLabels: {
        enabled: false,
      },

      yaxis: {
        labels: {
          trim: true, // don’t cut with ellipses
          maxWidth: 160, // increase if your names are longer

          // offsetX: 0,
          style: { fontSize: "0.875rem", fontWeight: "300" },
        },
      },
      xaxis: {
        type: "category" as const,
        tickPlacement: "between",
        labels: {
          trim: false,
          style: { fontSize: "0.875rem", fontWeight: "300" },
        },
      },
      stroke: {
        width: 6,
        colors: [getCssVarValue("--color-git-bg-bottom")],
      },
      tooltip: {
        y: {
          formatter: function (
            value,
            { series, seriesIndex, dataPointIndex, w }
          ) {
            const rawY = w.config.series[seriesIndex].data[dataPointIndex].raw;
            return rawY !== undefined ? rawY : value;
          },
        },
      },
      legend: {
        show: true,
        position: "bottom", // bottom of the chart
        horizontalAlign: "right", // align to left side
        formatter: () => "", // hide text, show only markers
        markers: {
          size: 16,
          shape: "square",
          radius: 10,
          strokeWidth: 0,
        },
        itemMargin: {
          horizontal: 2,
          vertical: 0,
        },
      },
    }),
    []
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
        height={500}
      />
    </GraphCard>
  );
}
