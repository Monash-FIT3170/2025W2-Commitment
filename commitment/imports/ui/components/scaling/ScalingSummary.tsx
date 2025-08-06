"use client";

import React, { useMemo } from "react";
import { DataTable } from "./ScalingTable";
import { ColumnDef } from "@tanstack/react-table";
import { ScalingRadialChart } from "./ScalingRadialChart";

type AliasEmail = {
  username: string;
  email: string;
};

type UserScalingSummary = {
  name: string;
  aliases: AliasEmail[];
  finalGrade: number;
  scale: number;
};

interface ScalingSummaryProps {
  method: string;
  metrics: string;
  fileName: string;
}

const ScalingSummary: React.FC<ScalingSummaryProps> = ({ method, metrics, fileName }) => {
  const userScalingSummaries: UserScalingSummary[] = useMemo(() => [
    {
      name: "Jupyta Notebuk",
      aliases: [
        { username: "Bobert", email: "bobert@example.com" },
        { username: "john", email: "john@example.com" },
      ],
      finalGrade: 78,
      scale: 1,
    },
    {
      name: "Poppy Willis",
      aliases: [
        { username: "capn america", email: "cap@example.com" },
        { username: "iyan man", email: "iyan@example.com" },
      ],
      finalGrade: 42,
      scale: 0.66,
    },
  ], []);

  const columns: ColumnDef<UserScalingSummary>[] = useMemo(() => [
    {
      accessorKey: "name",
      header: "Name",
      cell: ({ row }) => row.getValue("name"),
    },
    {
      accessorKey: "finalGrade",
      header: "Final Grade",
      cell: ({ row }) => row.getValue("finalGrade"),
    },
    {
    accessorKey: "scale",
    header: "Scale",
    cell: ({ row }) => (
        <ScalingRadialChart value={row.getValue("scale")} /> 
    ),
    }, 

  ], []);

  return (
    <div className="mt-8 p-4 border rounded-md bg-gray-50">
      <h2 className="text-xl font-semibold mb-2">Scaling Summary</h2>
      <p><strong>Method:</strong> {method}</p>
      <p><strong>Metrics:</strong> {metrics}</p>
      <p><strong>Grading Sheet:</strong> {fileName}</p>

      <div className="container mx-auto py-10">
        <DataTable columns={columns} data={userScalingSummaries} />
      </div>

    </div>
  );
};

export default ScalingSummary;

