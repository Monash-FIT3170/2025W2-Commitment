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
    <div className="overflow-hidden rounded-md border">
      <Table>
        <TableHeader>
          {table.getHeaderGroups().map((headerGroup) => (
            <TableRow key={headerGroup.id}>
              {headerGroup.headers.map((header) => (
                <TableHead key={header.id}>

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
                <TableRow
                onClick={() => row.toggleExpanded()}
                className="bg-git-int-primary hover:bg-git-int-primary text-white"
                >

                {row.getVisibleCells().map((cell) => (
                  <TableCell key={cell.id} >
                    {flexRender(cell.column.columnDef.cell, cell.getContext())}
                  </TableCell>
                ))}
              </TableRow>

              {row.getIsExpanded() && row.original.aliases?.length > 0 && (
                <>
                  
                  <TableRow className="bg-gray-50 border-b-2 border-black">
                    <TableCell colSpan={columns.length}>
                      <span className="ml-4 font-medium text-gray-800">
                        Associated Accounts
                      </span>
                    </TableCell>
                  </TableRow>

                  
                  {row.original.aliases.map((alias, idx) => (
                    <TableRow
                    key={`${row.id}-alias-${idx}`}
                    className="bg-gray-50 border-style: dashed">
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
