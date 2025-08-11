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
  //      An example entry
  //     {
  //     name: "Jupyta Notebuk",
  //     aliases: [
  //       { username: "Bobert", email: "bobert@example.com" },
  //       { username: "john", email: "john@example.com" },
  //     ],
  //     finalGrade: 78,
  //     scale: 1,
  //   }
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
    <div className="-mt-4 p-4 rounded-md bg-git-bg-elevated">
      <div className="max-h-[600px]  rounded-md bg-git-bg-elevated">
        <DataTable columns={columns} data={userScalingSummaries} />
      </div>
    </div>
  );
};

export default ScalingSummary;
