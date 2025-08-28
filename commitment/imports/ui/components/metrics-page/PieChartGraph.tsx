import React from "react";
import { PieChart, Pie, Cell, ResponsiveContainer, Tooltip } from "recharts";
import { TrendingUp } from "lucide-react";
import { CardContent, CardHeader, CardTitle } from "@ui/components/ui/card";
import InfoButton from "../ui/infoButton";
import GraphCard from "./GraphCard";
import { ContributionEntry } from "/imports/api/types";

interface PieProps {
  data: ContributionEntry[];
  title: string;
}

// CURRENT COLOUR PALLETTE - ASK PMs TO help

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

// const pieChartDescription =
//   "Commit distribution by contributor — each slice shows a contributor's share of total commits.";

// For pop up
// function CustomTooltip({
//   active,
//   payload,
// }: {
//   active?: boolean;
//   payload?: any[];
// }) {
//   if (!active || !payload || !payload.length) return null;

//   const { user, contributions, fill } = payload[0].payload;

//   return (
//     <div className="rounded-md border-2 border-black bg-white px-3 py-2 text-sm shadow-md text-muted-foreground ">
//       <div className="flex items-center gap-2 font-semibold">
//         <span
//           className="inline-block h-3 w-3 rounded-sm"
//           style={{ backgroundColor: fill }}
//         />
//         {user}
//       </div>
//       <div>{contributions} contributions</div>
//     </div>
//   );
// }

// Main Pie Chart
export const ContributionPieChart: React.FC<PieProps> = ({ data, title }) => {
  if (!data || data.length === 0) return null;

  const contributors: Record<string, number> = {};

  data.forEach(({ name, count }) => {
    contributors[name] = (contributors[name] || 0) + count;
  });

  const totalCommit = Object.values(contributors).reduce(
    (sum, val) => sum + val,
    0
  );

  const pieData = Object.entries(contributors).map(([name, value]) => ({
    name,
    value: parseFloat(((value / totalCommit) * 100).toFixed(2)),
  }));

  return (
    <GraphCard className="w-full max-w-[800px] h-[500px] min-w-[486px] flex flex-col basis-1/3">
      {/* Title */}
      <CardHeader className="pb-0">
        <CardTitle className="flex text-lg mt-0 font-bold">
          {title}
          <div className="relative -mt-3 ml-2">
            <InfoButton description="Shows contributor percentage based on total contributions." />
          </div>
        </CardTitle>
      </CardHeader>

      {/* Pie Chart */}
      <CardContent className="grow flex flex-col items-center justify-center pt-2">
        <ResponsiveContainer width="100%" height="100%">
          <PieChart>
            <Pie
              data={pieData}
              dataKey="value"
              nameKey="name"
              cx="50%"
              cy="50%"
              outerRadius={130}
              label={({ name, value }) => `${name}: ${value}%`}
            >
              {pieData.map((entry, index) => {
                const color =
                  staticColorPalette[index] ??
                  extendColorPalette(index - staticColorPalette.length);
                return <Cell key={entry.name} fill={color} />;
              })}
            </Pie>
            <Tooltip
              formatter={(value: number, name: string) => [`${value}%`, name]}
            />
          </PieChart>
        </ResponsiveContainer>
      </CardContent>
    </GraphCard>
  );
};

// export function ContributionPieChart({ data }: Props) {
//   const coloredData = data.map((entry, index) => ({
//     ...entry,
//     fill:
//       index < staticColorPalette.length
//         ? staticColorPalette[index]
//         : extendColorPalette(index - staticColorPalette.length),
//   }));
//   return (
//     <GraphCard className="w-full max-w-[800px] flex flex-col basis-1/3">
//       <CardHeader className="pb-0">
//         <div className="flex items-center space-x-2 w-4/5">
//           <h2 className="text-lg font-bold text-gray-800">Pie Chart</h2>

//           {/* Special margin for the infoButton to get it centred */}
//           <div className="relative -mt-2">
//             <InfoButton description={pieChartDescription} />
//           </div>
//         </div>

//         {/* <CardDescription>Last 6 months</CardDescription> */}
//       </CardHeader>

//       {coloredData.length === 0 ? (
//         <CardContent className="p-4 text-gray-500">
//           Please select an End Date in the Date Range
//         </CardContent>
//       ) : (
//         <>
//           <CardContent className="flex flex-col items-center gap-4">
//             {/* Pie */}
//             <PieChart width={300} height={300}>
//               <Pie
//                 data={coloredData}
//                 dataKey="contributions"
//                 nameKey="user"
//                 cx="50%"
//                 cy="50%"
//                 outerRadius={110}
//                 stroke="none"
//                 isAnimationActive
//                 animationDuration={800}
//                 labelLine={false}
//               >
//                 {coloredData.map((entry) => (
//                   <Cell key={entry.user} fill={entry.fill} stroke="none" />
//                 ))}
//               </Pie>
//               <Tooltip
//                 content={<CustomTooltip />}
//                 wrapperStyle={{ outline: "none" }}
//               />
//             </PieChart>

//             {/* Legend */}
//             <div className="w-full flex justify-center">
//               <ul className="inline-flex flex-wrap justify-center gap-x-4 gap-y-1 text-sm">
//                 {data.map((entry) => (
//                   <li key={entry.user} className="flex items-center gap-1">
//                     <span
//                       className="inline-block h-3 w-3 rounded-sm"
//                       style={{ backgroundColor: entry.fill }}
//                     />
//                     <span className="truncate">{entry.user}</span>
//                   </li>
//                 ))}
//               </ul>
//             </div>
//           </CardContent>

//           <CardFooter className="flex flex-col gap-1 text-sm text-muted-foreground">
//             <div className="flex items-center gap-2 font-medium leading-none">
//               Trending up by 5.2% this month
//               <TrendingUp className="h-4 w-4 text-green-500" />
//             </div>
//             <p className="leading-none">
//               Showing total contributions for the last 6 months
//             </p>
//           </CardFooter>
//         </>
//       )}
//     </GraphCard>
//   );
// }
