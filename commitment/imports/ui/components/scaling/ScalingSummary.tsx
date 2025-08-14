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
        cell: ({ row }) => row.getValue("finalGrade"),
      },
      {
        accessorKey: "scale",
        header: "Scale",
        cell: ({ row }) => <ScalingRadialChart value={row.getValue("scale")} />,
      },
    ],
    []
  );

  return (
    <div className="-mt-4  rounded-md bg-git-bg-elevated ">
      <div className="max-h-[600px]  rounded-md bg-git-bg-elevated">
        <DataTable columns={columns} data={userScalingSummaries} />
      </div>
    </div>
  );
};

export default ScalingSummary;
