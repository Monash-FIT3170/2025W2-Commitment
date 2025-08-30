import React from 'react';
import {
  BarChart,
  Bar,
  XAxis,
  YAxis,
  CartesianGrid,
  Tooltip,
  ResponsiveContainer,
  Cell,
} from 'recharts';
import InfoButton from '../ui/infoButton';
import GraphCard from './GraphCard';
import { CardHeader, CardContent } from '../ui/card';

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
  '#4E79A7',
  '#F28E2B',
  '#59A14F',
  '#E15759',
  '#76B7B2',
  '#EDC948',
  '#B07AA1',
  '#FF9DA7',
  '#9C755F',
  '#BAB0AC',
  '#D37295',
];

const extendColorPalette = (index: number): string => {
  const hue = (index * 137.508) % 360;
  return `hsl(${hue}, 70%, 55%)`;
};

export const LeaderboardGraph: React.FC<LeaderboardChartProps> = ({
  data,
  title,
}) => (
  <GraphCard className="h-[500px]">
    <CardHeader className="pb-0">
      <div className="pb-2 items-center flex ">
        <h2 className="text-lg font-bold">{title}</h2>
        <div className="-mt-2 ml-2">
          <InfoButton description="Shows top 5 contributors based on a given metric" />
        </div>
      </div>
    </CardHeader>

    <CardContent className="grow flex items-center justify-center">
      <ResponsiveContainer width="100%" height="100%">
        <BarChart
          layout="vertical"
          data={data}
          margin={{
            top: 20, right: 30, bottom: 20, left: -30,
          }}
          barCategoryGap="10%"
        >
          <CartesianGrid strokeDasharray="3 3" />
          <XAxis type="number" />
          <YAxis type="category" dataKey="name" width={100} />
          <Tooltip />
          <Bar dataKey="commits" barSize={30}>
            {data.map((_entry, index) => {
              const color = staticColorPalette[index]
                  ?? extendColorPalette(index - staticColorPalette.length);
              return <Cell key={`cell-${_entry.name}`} fill={color} />;
            })}
          </Bar>
        </BarChart>
      </ResponsiveContainer>
    </CardContent>
  </GraphCard>
);

export default LeaderboardGraph;
