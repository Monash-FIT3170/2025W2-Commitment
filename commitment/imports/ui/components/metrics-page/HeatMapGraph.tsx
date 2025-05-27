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

type ContributionDataItem = {
  name: string;
  date: string; // "yyyy-MM-dd"
  count: number;
};

interface HeatMapProps {
  data: ContributionDataItem[];
  startDate?: Date;
  endDate?: Date;
  maxUsersToShow?: number;
  title?: string; //This changes depending on the heatmap name - if it needs to change at all
}

const heatMapDescription =
  "Gain a visual insight on how contributors are performing within a certain period of time"; //FIX: Make a better heatmap description

// FIX: When the Product Managers define the colours in the system, use those instead of cardcoding
const levels = [
  "bg-orange-200",
  "bg-orange-300",
  "bg-orange-500",
  "bg-orange-600",
  "bg-orange-800",
];

const graphBackgroundColour = "#E8E8DD";

// Applying normalisation to the data -> the gradient is represented by the value
const getLevelClassNormalized = (ratio: number) => {
  if (ratio === 0) return levels[0];
  if (ratio < 0.25) return levels[1];
  if (ratio < 0.5) return levels[2];
  if (ratio < 0.75) return levels[3];
  return levels[4];
};

// Main heatmap function
export default function UserContributionHeatMap({
  data,
  startDate,
  endDate,
  maxUsersToShow,
  title,
}: HeatMapProps & { title?: string }) {
  //Mapping on many different properties - Days/ Weeks/ Months or Years

  // In the case where no data is given
  if (!data || data.length === 0) {
    return (
      <div
        className="rounded border p-6 overflow-x-auto font-mono " 
        style={{
          maxWidth: "fit-content", 
          backgroundColor: graphBackgroundColour,
        }}
      >
        {title && ( // could make this into a component
          <div className="flex items-center space-x-2 w-4/5 ">
            <h2 className="text-lg font-bold text-gray-800">{title}</h2>

            <div className="relative -mt-2 ">
              <InfoButton description={heatMapDescription} />
            </div>
          </div>
        )}

        <div className="p-4 text-gray-500">Please select and End Date in the Date Range</div>
      </div>
    );
  }

  const users = useMemo(
    () => Array.from(new Set(data.map((d) => d.name))).slice(0, maxUsersToShow),
    [data, maxUsersToShow]
  );

  const from = useMemo(() => {
    if (startDate) return startDate;
    return parseISO(
      data.reduce((min, d) => (d.date < min ? d.date : min), data[0]?.date)
    );
  }, [data, startDate]);

  const to = useMemo(() => {
    if (endDate) return endDate;
    return parseISO(
      data.reduce((max, d) => (d.date > max ? d.date : max), data[0]?.date)
    );
  }, [data, endDate]);

  const totalDays = differenceInCalendarDays(to, from) + 1;

  type Mode = "dayOfWeek" | "weeks" | "months" | "years";
  let mode: Mode;

  if (totalDays <= 7) {
    mode = "dayOfWeek";
  } else if (totalDays <= 28) {
    mode = "weeks";
  } else if (getYear(to) > getYear(from)) {
    mode = "years";
  } else {
    mode = "months";
  }

  const xLabels: string[] = [];
  let bucketsCount = 0;

  if (mode === "dayOfWeek") {
    xLabels.push(...["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"]);
    bucketsCount = 7;
  } else if (mode === "weeks") {
    const numWeeks = Math.min(4, Math.ceil(totalDays / 7));
    bucketsCount = numWeeks;
    for (let i = 0; i < numWeeks; i++) {
      const weekStart = addDays(from, i * 7);
      const weekEnd = addDays(weekStart, 6);
      xLabels.push(
        `${format(weekStart, "MMM d")} - ${format(
          weekEnd < to ? weekEnd : to,
          "MMM d"
        )}`
      );
    }
  } else if (mode === "months") {
    bucketsCount = 12;
    xLabels.push(
      "Jan",
      "Feb",
      "Mar",
      "Apr",
      "May",
      "Jun",
      "Jul",
      "Aug",
      "Sep",
      "Oct",
      "Nov",
      "Dec"
    );
  } else {
    const endYear = getYear(to);
    bucketsCount = 10;
    for (let i = 0; i < bucketsCount; i++) {
      xLabels.unshift((endYear - i).toString());
    }
  }

  const table: Record<string, number[]> = {};
  users.forEach((user) => {
    table[user] = Array(bucketsCount).fill(0);
  });

  const getBucketIndex = (dateStr: string): number => {
    const date = parseISO(dateStr);
    if (mode === "dayOfWeek") {
      return getDay(date);
    } else if (mode === "weeks") {
      const diffDays = differenceInCalendarDays(date, from);
      return Math.min(Math.floor(diffDays / 7), bucketsCount - 1);
    } else if (mode === "months") {
      return getMonth(date);
    } else {
      const year = getYear(date);
      const endYear = getYear(to);
      const idx = endYear - year;
      return idx >= 0 && idx < bucketsCount ? bucketsCount - 1 - idx : -1;
    }
  };

  data.forEach(({ name, date, count }) => {
    if (!users.includes(name)) return;
    const idx = getBucketIndex(date);
    if (idx >= 0 && idx < bucketsCount) {
      table[name][idx] += count;
    }
  });

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
  const gridTemplateColumns = `${labelWidth}px repeat(${bucketsCount}, ${cellSize}px)`;
  const gridTemplateRows = `repeat(${users.length}, ${cellSize}px) 1fr`;

  return (
    //   The things actually in this component
    <div
      className="rounded border p-6 overflow-x-auto font-mono " //could be issue here with padding
      style={{
        maxWidth: "fit-content", // limit width to content width
        backgroundColor: graphBackgroundColour,
      }}
    >
      {title && (
        <div className="flex items-center space-x-2 w-4/5 ">
          <h2 className="text-lg font-bold text-gray-800">{title}</h2>

          {/* Special margin for the infoButton to get it centred */}
          <div className="relative -mt-2 ">
            <InfoButton description={heatMapDescription} />
          </div>
        </div>
      )}

      {/* STYLING FOR HEATMAP OVERALL */}
      <div
        style={{
          display: "grid",
          gridTemplateColumns,
          gridTemplateRows,
          gap: `${spacing}px ${spacing}px`,
          boxSizing: "border-box",
          padding: "0",
          border: "1px solid #e5e7eb",
          borderRadius: "0.375rem",
          width: "fit-content",
          maxWidth: "100%",
          backgroundColor: graphBackgroundColour, // keep heatmap itself white
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
                key={`${user}-${colIdx}`}
                className={`rounded-sm cursor-default  ${getLevelClassNormalized(
                  ratio
                )}`}
                style={{
                  gridColumn: colIdx + 2,
                  gridRow: rowIdx + 1,
                  width: cellSize,
                  height: cellSize,
                }}
                title={`${user} · ${xLabels[colIdx]}: ${count} LOC (max: ${max})`}
              />
            );
          })
        )}

        {/* X AXIS LABELS */}
        {xLabels.map((label, idx) => (
          <div
            key={`x-label-${idx}`}
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
    </div>
  );
}
