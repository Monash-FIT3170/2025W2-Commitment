"use client"

import React from "react"
import { PieChart, Pie, Cell, Tooltip } from "recharts"

import { TrendingUp } from "lucide-react"

import {
  Card,
  CardContent,
  CardDescription,
  CardFooter,
  CardHeader,
  CardTitle,
} from "@ui/components/ui/card"
import InfoButton from "./infoButton"

export interface ChartEntry {
  user: string
  contributions: number
  fill: string
}

interface Props {
  data: ChartEntry[]
}

// CURRENT COLOUR PALLETTE - ASK PMs TO help

const graphBackgroundColour = "#E8E8DD"

const dark2 = [
  "#1b9e77", // teal‑green
  "#d95f02", // orange
  "#66a61e", // lime‑green
  "#e6ab02", // yellow‑orange
  "#a6761d", // brown‑orange
  "#666666", // grey
  "#7570b3", // purple
  "#e7298a", // pink
]
const CustomTooltip = ({
  active,
  payload,
}: {
  active?: boolean
  payload?: any[]
}) => {
  if (!active || !payload || !payload.length) return null

  const { user, contributions } = payload[0].payload

  return (
    <div className="rounded-md border bg-white px-3 py-2 text-sm shadow-md text-muted-foreground">
      <div className="font-semibold">{user}</div>
      <div>{contributions} contributions</div>
    </div>
  )
}





/* ---------- pie chart component ---------- */
export function GitHubContribPie({ data }: Props) {
  return (
    <Card className="flex flex-col max-w-md mx-auto" style={{ backgroundColor: graphBackgroundColour }}>


      <CardHeader className="items-center pb-0">


        <div className="flex items-center space-x-2 w-4/5">
                <h2 className="text-lg font-bold text-gray-800">{"GitHub Contributions"}</h2>

                {/* Special margin for the infoButton to get it centred */}
                <div className="relative -mt-2">
                    <InfoButton description={"Contributions"} />
                </div>
        </div>



        <CardDescription>Last 6 months</CardDescription>
      </CardHeader>

      <CardContent className="flex flex-col items-center gap-4">
        {/* Pie */}
        <PieChart width={260} height={260}>
  <Pie
    data={data}
    dataKey="contributions"
    nameKey="user"
    cx="50%"
    cy="50%"
    outerRadius={90}
    stroke="none"
    isAnimationActive
    animationDuration={800}
    labelLine={false}
  >
    {data.map((entry, i) => (
      <Cell
        key={`cell-${i}`}
        fill={entry.fill}
        stroke={entry.fill}
        strokeWidth={1}
      />
    ))}
  </Pie>
  <Tooltip content={<CustomTooltip />} wrapperStyle={{ outline: "none" }} />

</PieChart>


        {/* Legend */}
        {/* <div className="w-full flex justify-center">
        <ul className="inline-flex flex-wrap justify-center gap-x-4 gap-y-1 text-sm">
        {data.map((entry, i) => (
        <li
        key={`legend-${i}`}
        className="flex items-center gap-1"
        >
        <span
            className="inline-block h-3 w-3 rounded-sm"
            style={{ backgroundColor: entry.fill }}
        />
        <span className="truncate">{entry.user}</span>
        </li>
        ))}
        </ul>
        </div> */}

      </CardContent>

      <CardFooter className="flex flex-col gap-1 text-sm text-muted-foreground">
        <div className="flex items-center gap-2 font-medium leading-none">
          Trending up by 5.2 % this month
          <TrendingUp className="h-4 w-4 text-green-500" />
        </div>
        <p className="leading-none">
          Showing total contributions for the last 6 months
        </p>
      </CardFooter>
    </Card>
  )
}

/* ---------- dummy‑data generator ---------- */
export function generateDummyGitHubData(count: number): ChartEntry[] {
  return Array.from({ length: count }, (_, i) => ({
    user: `user${i + 1}`,
    contributions: Math.floor(Math.random() * 400) + 50,
    fill: dark2[i % dark2.length],
  }))
}

