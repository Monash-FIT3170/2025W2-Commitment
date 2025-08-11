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

type AliasEmail = {
  username: string;
  email: string;
};

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
    getRowCanExpand: () => true,
  });

  return (
    <div className="overflow-hidden rounded-md  bg-git-bg-elevated">
      <Table>
       <TableHeader>
        {table.getHeaderGroups().map((headerGroup) => (
            <TableRow key={headerGroup.id}>
            {headerGroup.headers.map((header) => (
                <TableHead key={header.id} className="font-bold">
                {header.isPlaceholder
                    ? null
                    : flexRender(header.column.columnDef.header, header.getContext())}
                </TableHead>
            ))}
            </TableRow>
        ))}
        </TableHeader>

        <TableBody>
          {table.getRowModel().rows.map((row) => (
            <React.Fragment key={row.id}>
                <TableRow onClick={() => row.toggleExpanded()} className="cursor-pointer">
                {row.getVisibleCells().map((cell, idx) => (
                    <TableCell
                    key={cell.id}
                    className={
                        (idx === 0
                        ? "rounded-l-md bg-git-int-primary hover:bg-git-int-primary text-white text-1xl"
                        : idx === row.getVisibleCells().length - 1
                        ? "rounded-r-md bg-git-int-primary hover:bg-git-int-primary text-white"
                        : "bg-git-int-primary hover:bg-git-int-primary text-white text-2xl font-bold") +
                        " py-0"
                    }
                    >

                    {flexRender(cell.column.columnDef.cell, cell.getContext())}
                    {idx === row.getVisibleCells().length - 2 && "%"}
                    </TableCell>
                ))}
                </TableRow>

              {row.getIsExpanded() && row.original.aliases?.length > 0 && (
                <>
                  
                  <TableRow className="bg-git-bg-elevated hover:bg-git-bg-elevated text-white border-black">
                    <TableCell colSpan={columns.length}>
                      <span className="ml-4 font-medium text-gray-800">
                        Associated Accounts
                      </span>
                    </TableCell>
                  </TableRow>

                  
                  {row.original.aliases.map((alias, idx) => (
                    <TableRow
                    key={`${row.id}-alias-${idx}`}
                    className="bg-git-bg-elevated hover:bg-git-bg-elevated border-style: dashed">
                    <TableCell colSpan={columns.length}>
                        <div className="ml-8 text-sm text-gray-700">
                        <strong>{alias.username}</strong> ({alias.email})
                        </div>
                    </TableCell>
                    </TableRow>
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
