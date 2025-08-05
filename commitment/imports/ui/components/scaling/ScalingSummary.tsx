"use client"


import React, { useMemo } from "react";
import { DataTable } from "./ScalingTable";
import { ColumnDef } from "@tanstack/react-table";

type UserScalingSummary = {
  name: string;
  aliases: string[];
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
      aliases: ["Bobert", "john"],
      finalGrade: 78,
      scale: 1.2,
    },
    {
      name: "Poppy Willis",
      aliases: ["capn america", "iyan man"],
      finalGrade: 42,
      scale: 0.8,
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
      cell: ({ row }) => row.getValue("scale"),
    },
    {
      accessorKey: "aliases",
      header: "Aliases",
      cell: ({ row }) => {
        const aliases = row.getValue("aliases");
        return Array.isArray(aliases) ? aliases.join(", ") : "—";
      },
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
"use client"
