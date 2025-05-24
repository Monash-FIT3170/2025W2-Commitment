"use client"

import * as React from "react"
import { Button } from "@ui/components/ui/button"
import {
  DropdownMenu,
  DropdownMenuCheckboxItem,
  DropdownMenuContent,
  DropdownMenuLabel,
  DropdownMenuSeparator,
  DropdownMenuTrigger,
} from "@ui/components/ui/dropdown-menu"

interface DropdownMenuCheckboxesProps {
  branches: string[]
}

export function BranchDropDownMenu({ branches }: DropdownMenuCheckboxesProps) {
  const [selectedBranch, setSelectedBranch] = React.useState<string | null>(
    branches.length > 0 ? branches[0] : null
  );

  return (
    <DropdownMenu>
      <DropdownMenuTrigger asChild>
        <Button 
          variant="outline"
          className="w-[280px] justify-start focus:outline-none focus:ring-0 border-2 border-black"
        >
          {selectedBranch ?? "Select a branch"}
        </Button>
      </DropdownMenuTrigger>
      <DropdownMenuContent className="w-[280px] focus:outline-none focus:ring-0 border-2 border-black">
        {/* <DropdownMenuLabel>Select Branch</DropdownMenuLabel> */}
        <DropdownMenuSeparator />
        {branches.map(branch => (
          <DropdownMenuCheckboxItem
            key={branch}
            checked={selectedBranch === branch}
            onCheckedChange={() => setSelectedBranch(branch)}
          >
            {branch}
          </DropdownMenuCheckboxItem>
        ))}
      </DropdownMenuContent>
    </DropdownMenu>
  )
}
