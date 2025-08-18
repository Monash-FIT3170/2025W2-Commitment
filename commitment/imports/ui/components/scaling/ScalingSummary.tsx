"use client";

import React, { useMemo } from "react";
import { DataTable } from "./ScalingTable";
import { ColumnDef } from "@tanstack/react-table";
import { ScalingRadialChart } from "./ScalingRadialChart";
import type { UserScalingSummary } from "@server/commitment_api/types";

interface ScalingSummaryProps {
  userScalingSummaries: UserScalingSummary[];
}

/**
 *
 * @param  userScalingSummaries contain the details of each person, their aliases with emails, their grade and the scaling applied to their grade.
 * @returns
 */
const ScalingSummary: React.FC<ScalingSummaryProps> = ({
  userScalingSummaries,
}) => {
  const columns: ColumnDef<UserScalingSummary>[] = useMemo(
    () => [
      {
        accessorKey: "name",
        header: "Contributor Name",
        cell: ({ row }) => row.getValue("name"),
      },
      {
        accessorKey: "finalGrade",
        header: "Final Grade",
        cell: ({ row }) => {
          const grade = row.getValue("finalGrade") as number | null | undefined;
          return grade ?? 0; //a grade of 0 as we can't assume any grade
        },
      },
      {
        accessorKey: "scale",
        header: "Scale",
        cell: ({ row }) => {
          const scale = row.getValue("scale") as number | null | undefined;
          return <ScalingRadialChart value={scale ?? 1} />; //a scale of 1 as no scaling has been applied
        },
      },
    ],
    []
  );

  return (
    <div className="-mt-4  rounded-md bg-git-bg-elevated overflow-scroll ">
      <div className="max-h-[600px]  rounded-md bg-git-bg-elevated">
        <DataTable columns={columns} data={userScalingSummaries} />
      </div>
    </div>
  );
};

export default ScalingSummary;
