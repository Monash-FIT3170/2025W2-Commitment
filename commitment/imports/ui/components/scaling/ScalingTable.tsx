import React from "react";
import {
  ColumnDef,
  flexRender,
  getCoreRowModel,
  getExpandedRowModel,
  useReactTable,
} from "@tanstack/react-table";

import {
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableHeader,
  TableRow,
} from "@ui/components/ui/table";

import { ChevronRight } from "lucide-react";

import InfoButton from "../ui/infoButton";
import { AliasEmail } from "/imports/api/types";

interface DataTableProps<TData extends { aliases?: AliasEmail[] }, TValue> {
  columns: ColumnDef<TData, TValue>[];
  data: TData[];
}

export function DataTable<TData extends { aliases?: AliasEmail[] }, TValue>({
  columns,
  data,
}: DataTableProps<TData, TValue>) {
  const table = useReactTable({
    data,
    columns,
    getCoreRowModel: getCoreRowModel(),
    getExpandedRowModel: getExpandedRowModel(),
    getRowCanExpand: (row) => (row.original.aliases?.length ?? 0) > 0,
  });

  return (
    <div className="overflow-hidden rounded-md border-git-stroke-primary">
      <Table>
        {/* Table Header */}
        <TableHeader>
          {table.getHeaderGroups().map((headerGroup) => (
            <TableRow key={headerGroup.id}>
              {headerGroup.headers.map((header) => (
                <TableHead
                  key={header.id}
                  className="text-git-text-primary bg-git-int-secondary hover:bg-git-int-secondary"
                >
                  {header.isPlaceholder
                    ? null
                    : flexRender(
                        header.column.columnDef.header,
                        header.getContext()
                      )}
                </TableHead>
              ))}
            </TableRow>
          ))}
        </TableHeader>

        {/* Remainder of Table */}
        <TableBody>
          {table.getRowModel().rows.map((row) => (
            <React.Fragment key={row.id}>
              {/* Unexpanded Rows */}
              <TableRow
                onClick={() => row.toggleExpanded()}
                className="cursor-pointer"
              >
                {row.getVisibleCells().map((cell, idx) => (
                  <TableCell
                    key={cell.id}
                    className={
                      (idx === 0
                        ? "rounded-l-md bg-git-int-primary hover:bg-git-int-primary text-git-int-text text-1xl"
                        : idx === row.getVisibleCells().length - 1
                        ? "rounded-r-md bg-git-int-primary hover:bg-git-int-primary text-git-int-text"
                        : "bg-git-int-primary hover:bg-git-int-primary text-git-int-text text-2xl font-bold") +
                      " py-0"
                    }
                  >
                    {idx === 0 ? (
                      <div className="flex items-center gap-1">
                        {/* Chevron or placeholder */}
                        {row.getCanExpand() ? (
                          <ChevronRight
                            className={`transition-transform duration-200 ${
                              row.getIsExpanded() ? "rotate-90" : ""
                            }`}
                            size={16}
                          />
                        ) : (
                          <span className="w-4" />
                        )}

                        {/* Name */}
                        <span className="leading-none">
                          {flexRender(
                            cell.column.columnDef.cell,
                            cell.getContext()
                          )}
                        </span>

                        {/* InfoButton if no aliases */}
                        {(!row.original.aliases ||
                          row.original.aliases.length === 0) && (
                          <div className="flex items-center -translate-y-2 pl-0.5">
                            <InfoButton description="This contributor has not contributed to the Main/Master branch!" />
                          </div>
                        )}
                      </div>
                    ) : (
                      <>
                        {flexRender(
                          cell.column.columnDef.cell,
                          cell.getContext()
                        )}
                        {idx === row.getVisibleCells().length - 2 &&
                          typeof cell.getValue() === "number" &&
                          "%"}
                      </>
                    )}
                  </TableCell>
                ))}
              </TableRow>

              {/* Expanded Rows */}
              {row.getIsExpanded() && row.original.aliases?.length > 0 && (
                <>
                  {/* Row for Associated Accounts */}

                  <TableRow className="bg-git-int-secondary hover:bg-git-int-secondary !border-0 px-4">
                    <TableCell
                      colSpan={columns.length}
                      className="!border-0 p-0"
                    >
                      {/* Label */}
                      <div className="px-4 py-2">
                        <span className="ml-4 font-medium text-git-text-secondary">
                          Associated Accounts
                        </span>
                      </div>

                      {/* Inset solid black separator */}
                      <div className=" border-b-2 border-git-stroke-primary mx-5" />
                    </TableCell>
                  </TableRow>

                  {/* Rows for Aliases */}
                  {row.original.aliases.map((alias, idx) => (
                    <React.Fragment key={`${row.id}-alias-${idx}`}>
                      <TableRow className="bg-git-int-secondary hover:bg-git-int-secondary !border-0">
                        <TableCell
                          colSpan={columns.length}
                          className="!border-0 px-4"
                        >
                          <div className="ml-4 text-sm text-git-text-secondary">
                            <strong>{alias.email}</strong>
                          </div>
                        </TableCell>
                      </TableRow>

                      {/* Dashed separator, only if not last alias */}
                      {idx < row.original.aliases.length - 1 && (
                        <TableRow className="!border-0">
                          <TableCell
                            colSpan={columns.length}
                            className="!border-0 px-4"
                          >
                            <div className="h-0.5 border-t-2 border-dashed border-git-stroke-primary mx-1" />
                          </TableCell>
                        </TableRow>
                      )}
                    </React.Fragment>
                  ))}
                </>
              )}
            </React.Fragment>
          ))}
        </TableBody>
      </Table>
    </div>
  );
}
