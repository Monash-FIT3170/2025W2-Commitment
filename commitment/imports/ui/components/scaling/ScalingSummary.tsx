"use client";

import React, { useMemo } from "react";
import { CellContext, ColumnDef } from "@tanstack/react-table";
import { DataTable } from "./ScalingTable";
import { ScalingRadialChart } from "./ScalingRadialChart";
import { UserScalingSummary } from "@api/types";

interface ScalingSummaryProps {
  userScalingSummaries: UserScalingSummary[]; 
  hasGradingSheet: boolean;
}

const renderNameCell = (cell: CellContext<UserScalingSummary, unknown>) => (
  <span className="text-git-int-text text-sm font-normal">
    {cell.getValue<string>()}
  </span>
);

const renderScaleCell = (cell: CellContext<UserScalingSummary, unknown>) => (
  <ScalingRadialChart value={cell.getValue<number>()} />
);

const ScalingSummary: React.FC<ScalingSummaryProps> = ({
  userScalingSummaries,
  hasGradingSheet,
}) => {
  const columns: ColumnDef<UserScalingSummary>[] = useMemo(() => {
    const baseCols: ColumnDef<UserScalingSummary>[] = [
      {
        accessorKey: "name",
        header: "Contributor Name",
        cell: renderNameCell,
      },
      {
        accessorKey: "scale",
        header: "Scale",
        cell: renderScaleCell,
      },
    ];

    // Insert Final Grade only if grading sheet exists
    if (hasGradingSheet) {
      baseCols.splice(1, 0, {
        accessorKey: "finalGrade",
        header: "Final Grade",
        cell: ({ row }) => {
          const grade = row.getValue("finalGrade");
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
