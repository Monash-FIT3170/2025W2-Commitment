import React from "react";
import InfoButton from "../ui/infoButton";
import {
  BarChart,
  Bar,
  XAxis,
  YAxis,
  CartesianGrid,
  Tooltip,
  ResponsiveContainer,
  Cell,
} from "recharts";
import GraphCard from "./GraphCard";

// Type for each contributor's data
interface TopContributor {
  name: string;
  commits: number;
}

interface LeaderboardChartProps {
  data: TopContributor[];
  title: string;
  xAxisLabel?: string;
}

const staticColorPalette = [
  "#4E79A7",
  "#F28E2B",
  "#59A14F",
  "#E15759",
  "#76B7B2",
  "#EDC948",
  "#B07AA1",
  "#FF9DA7",
  "#9C755F",
  "#BAB0AC",
  "#D37295",
];

const extendColorPalette = (index: number): string => {
  const hue = (index * 137.508) % 360;
  return `hsl(${hue}, 70%, 55%)`;
};

export const LeaderboardGraph: React.FC<LeaderboardChartProps> = ({
  data,
  title,
}) => {
  return (
    <GraphCard className="w-full max-w-[600px] h-[400px] flex flex-col basis-1/3">
      <div className="pb-2 items-center flex justify-between">
        <h2 className="text-lg font-bold">{title}</h2>
        <div className="-mt-2">
          <InfoButton description="Shows top 5 contributors based on a given metric" />
        </div>
      </div>

      <div className="flex-grow pt-0">
        <ResponsiveContainer width="100%" height="100%">
          <BarChart
            layout="vertical"
            data={data}
            margin={{ top: 20, right: 30, bottom: 20, left: -30 }}
            barCategoryGap="10%"
          >
            <CartesianGrid strokeDasharray="3 3" />
            <XAxis type="number" />
            <YAxis type="category" dataKey="name" width={100} />
            <Tooltip />
            <Bar dataKey="commits" barSize={30}>
              {data.map((_entry, index) => {
                const color =
                  staticColorPalette[index] ??
                  extendColorPalette(index - staticColorPalette.length);
                return <Cell key={`cell-${index}`} fill={color} />;
              })}
            </Bar>
          </BarChart>
        </ResponsiveContainer>
      </div>
    </GraphCard>
  );
};

export default LeaderboardGraph;
