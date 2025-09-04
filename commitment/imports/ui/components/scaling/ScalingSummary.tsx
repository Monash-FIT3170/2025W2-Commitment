"use client";

import React, { useMemo } from "react";
import { DataTable } from "./ScalingTable";
import { ColumnDef } from "@tanstack/react-table";
import { ScalingRadialChart } from "./ScalingRadialChart";
import { UserScalingSummary } from "/imports/api/types";
interface ScalingSummaryProps {
  userScalingSummaries: UserScalingSummary[]; //IF A GRADING SHEET IS PROVIDED, THE VALUES OF THIS PARAMETER MUST REFLECT THE FINAL GRADE, THUS CALCULATIONS ARE DONE AT THE GradingSheetForm STAGE
  hasGradingSheet: boolean;
}

const ScalingSummary: React.FC<ScalingSummaryProps> = ({
  userScalingSummaries,
  hasGradingSheet,
}) => {
  const columns: ColumnDef<UserScalingSummary>[] = useMemo(() => {
    const baseCols: ColumnDef<UserScalingSummary>[] = [
      {
        accessorKey: "name",
        header: "Contributor Name",
        cell: ({ row }) => (
          <span className="text-git-int-text text-sm font-normal">
            {row.getValue("name")}
          </span>
        ),
      },
      {
        accessorKey: "scale",
        header: "Scale",
        cell: ({ row }) => (
          <ScalingRadialChart value={row.getValue("scale") as number} />
        ),
      },
    ];

    // Insert Final Grade only if grading sheet exists
    if (hasGradingSheet) {
      baseCols.splice(1, 0, {
        accessorKey: "finalGrade",
        header: "Final Grade",
        cell: ({ row }) => {
          const grade = row.getValue("finalGrade") as number | null;
          return grade === null ? "-" : grade;
        },
      });
    }

    return baseCols;
  }, [hasGradingSheet]);

  return (
    <div className="-mt-4 rounded-md bg-git-bg-elevated overflow-scroll">
      <div className="max-h-[600px] rounded-md bg-git-bg-elevated">
        <DataTable columns={columns} data={userScalingSummaries} />
      </div>
    </div>
  );
};

export default ScalingSummary;