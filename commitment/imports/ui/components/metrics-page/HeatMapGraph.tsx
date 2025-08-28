import React, { useMemo } from "react";
import {
  format,
  parseISO,
  differenceInCalendarDays,
  addDays,
  getDay,
  getMonth,
  getYear,
} from "date-fns";
import InfoButton from "../ui/infoButton";
import GraphCard from "./GraphCard";
import { CardHeader, CardTitle, CardContent } from "../ui/card";
import { ContributionEntry } from "/imports/api/types";

interface HeatmapProps {
  data: ContributionEntry[];
  maxUsersToShow?: number;
  title?: string;
}

const heatMapDescription =
  "Gain a visual insight on how contributors are performing within a certain period of time";

const levels = [
  "bg-git-200",
  "bg-git-300",
  "bg-git-500",
  "bg-git-600",
  "bg-git-800",
];

// Applying normalisation to the data -> the gradient is represented by the value
const getLevelClassNormalized = (ratio: number) => {
  if (ratio === 0) return levels[0];
  if (ratio < 0.25) return levels[1];
  if (ratio < 0.5) return levels[2];
  if (ratio < 0.75) return levels[3];
  return levels[4];
};

// Main heatmap function
export default function HeatmapGraph({
  data,
  maxUsersToShow,
  title,
}: HeatmapProps): React.ReactElement {
  // Mapping on many different properties - Days/ Weeks/ Months or Years
  const users = useMemo(
    () => Array.from(new Set(data.map((d) => d.name))).slice(0, maxUsersToShow),
    [data, maxUsersToShow]
  );

  let from: Date = useMemo(
    () =>
      parseISO(
        data.reduce((min, d) => (d.date < min ? d.date : min), data[0].date)
      ),
    [data]
  );

  const to: Date = useMemo(
    () =>
      parseISO(
        data.reduce((max, d) => (d.date > max ? d.date : max), data[0].date)
      ),
    [data]
  );

  const totalDays = differenceInCalendarDays(to, from) + 1;

  type Mode = "dayOfWeek" | "weeks" | "months" | "years";
  let mode: Mode;

  if (totalDays <= 7) {
    mode = "dayOfWeek";
  } else if (totalDays <= 49) {
    mode = "weeks";
  } else if (getYear(to) > getYear(from)) {
    mode = "years";
  } else {
    mode = "months";
  }

  const yAxisLabels: string[] = [];
  let yAxisLength = 7;

  if (mode === "dayOfWeek") {
    yAxisLabels.push(...["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"]);
  } else if (mode === "weeks") {
    const numWeeks = Math.min(7, Math.ceil(totalDays / 7));

    from = addDays(from, -7 * (7 - numWeeks));

    for (let i = 0; i < yAxisLength; i++) {
      const weekStarts = addDays(from, i * 7);
      yAxisLabels.push(`${format(weekStarts, "MMM d")}`);
    }
  } else if (mode === "months") {
    // Only show last 7 months with data
    const startMonth = getMonth(from);
    const endMonth = getMonth(to);
    const startYear = getYear(from);
    const endYear = getYear(to);

    // Collect all months between from and to
    const months: { year: number; month: number }[] = [];
    let y = startYear;
    let m = startMonth;
    while (y < endYear || (y === endYear && m <= endMonth)) {
      months.push({ year: y, month: m });
      m++;
      if (m > 11) {
        m = 0;
        y++;
      }
    }
    const lastMonths = months.slice(-7);
    while (lastMonths.length < 7) {
      const first = lastMonths[0];
      let prevMonth = first.month - 1;
      let prevYear = first.year;
      if (prevMonth < 0) {
        prevMonth = 11;
        prevYear -= 1;
      }
      lastMonths.unshift({ year: prevYear, month: prevMonth });
    }
    lastMonths.forEach(({ year, month }) => {
      yAxisLabels.push(`${format(new Date(year, month, 1), "MMM yyyy")}`);
    });
  } else {
    const endYear = getYear(to);
    for (let i = 0; i < yAxisLength; i++) {
      yAxisLabels.unshift((endYear - i).toString());
    }
  }

  const table: Record<string, number[]> = useMemo(() => {
    const tbl: Record<string, number[]> = {};
    users.forEach((user) => {
      tbl[user] = Array<number>(yAxisLength).fill(0);
    });

    const getBucketIndex = (dateStr: string): number => {
      const date = parseISO(dateStr);
      if (mode === "dayOfWeek") {
        return getDay(date);
      }
      if (mode === "weeks") {
        const diffDays = differenceInCalendarDays(date, from);
        return Math.min(Math.floor(diffDays / 7), yAxisLength - 1);
      }
      if (mode === "months") {
        return getMonth(date);
      }
      const year = getYear(date);
      const endYear = getYear(to);
      const idx = endYear - year;
      return idx >= 0 && idx < yAxisLength ? yAxisLength - 1 - idx : -1;
    };

    data.forEach(({ name, date, count }) => {
      if (!users.includes(name)) return;
      const idx = getBucketIndex(date);
      if (idx >= 0 && idx < yAxisLength) {
        tbl[name][idx] += count;
      }
    });

    return tbl;
  }, [users, yAxisLength, mode, from, to, data]);

  const userMaxMap = useMemo(() => {
    const map: Record<string, number> = {};
    users.forEach((user) => {
      map[user] = Math.max(...table[user]);
    });
    return map;
  }, [table, users]);

  // Cell styling

  const cellSize = 40;
  const spacing = 8;
  const labelWidth = 50;
  const gridTemplateColumns = `${labelWidth}px repeat(${yAxisLength}, ${cellSize}px)`;
  const gridTemplateRows = `repeat(${users.length}, ${cellSize}px) 1fr`;

  return (
    //   The things actually in this component
    <GraphCard className="w-full max-w-[800px] min-w-[486px] flex flex-col basis-1/3">
      <CardHeader className="pb-0">
        <CardTitle className="flex text-lg mt-0 font-bold ">
          {title}
          <div className="relative -mt-3 ml-2">
            <InfoButton description={heatMapDescription} />
          </div>
        </CardTitle>
      </CardHeader>

      <CardContent className="grow flex flex-col items-center justify-center pt-2">
        {/* STYLING FOR HEATMAP OVERALL */}
        <div
          style={{
            display: "grid",
            gridTemplateColumns,
            gridTemplateRows,
            gap: `${spacing}px ${spacing}px`,
            boxSizing: "border-box",
            padding: "0",
            width: "fit-content",
            maxWidth: "100%",
          }}
        >
          {/*  Y AXIS LABELS */}
          {users.map((user, idx) => (
            <div
              key={user}
              style={{
                gridColumn: 1,
                gridRow: idx + 1,
                height: cellSize,
                lineHeight: `${cellSize}px`,
                overflow: "hidden",
              }}
              className="text-sm text-gray-700 truncate font-semibold "
              title={user}
            >
              {user}
            </div>
          ))}
          {/* MAP USERS TO ROWS */}
          {users.map((user, rowIdx) =>
            table[user].map((count, colIdx) => {
              const max = userMaxMap[user];
              const ratio = max === 0 ? 0 : count / max;
              return (
                <div
                  key={`${user}-${yAxisLabels[colIdx]}`}
                  className={`rounded-sm cursor-default  ${getLevelClassNormalized(
                    ratio
                  )}`}
                  style={{
                    gridColumn: colIdx + 2,
                    gridRow: rowIdx + 1,
                    width: cellSize,
                    height: cellSize,
                  }}
                  title={`${user} · ${yAxisLabels[colIdx]}: ${count} LOC`}
                />
              );
            })
          )}
          {/* X AXIS LABELS */}
          {yAxisLabels.map((label, idx) => (
            <div
              key={`x-label-${label}`}
              style={{
                gridColumn: idx + 2,
                gridRow: users.length + 1,
                width: cellSize,
                textAlign: "center",
                fontWeight: 600,
                fontSize: "0.875rem",
                color: "#4B5563",
              }}
            >
              {label}
            </div>
          ))}
        </div>
      </CardContent>
    </GraphCard>
  );
}
