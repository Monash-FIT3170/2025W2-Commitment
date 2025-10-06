import * as React from "react";
import { Button } from "@base/button";
import {
  DropdownMenu,
  DropdownMenuCheckboxItem,
  DropdownMenuContent,
  DropdownMenuTrigger,
} from "@base/dropdown-menu";
import { ChevronDown } from "lucide-react";

interface DropdownMenuCheckboxesProps {
  branches: string[];
  selected: string | undefined;
  onChange: (selected: string) => void;
}

export default function BranchDropdownMenu({
  branches,
  selected,
  onChange,
}: DropdownMenuCheckboxesProps): React.JSX.Element {
  return (
    <DropdownMenu modal={false}>
      <DropdownMenuTrigger asChild>
        <Button
          variant="outline"
          className="w-[300px] justify-start text-left font-normal border-2 rounded-lg border-git-stroke-primary/40"
        >
          <span className="w-full truncate text-left flex justify-between items-center">
            {selected ?? "Select a branch"}
            <ChevronDown className="h-4 w-4 opacity-50" />
          </span>
        </Button>
      </DropdownMenuTrigger>
      <DropdownMenuContent className="w-[280px]  focus:ring-0 border-2 border-git-stroke-primary/40">
        {/* <DropdownMenuLabel>Select Branch</DropdownMenuLabel> */}
        {/* <DropdownMenuSeparator /> */}
        {branches.map((branch) => (
          <DropdownMenuCheckboxItem
            key={branch}
            checked={selected === branch}
            onCheckedChange={() => onChange(branch)}
          >
            {branch}
          </DropdownMenuCheckboxItem>
        ))}
      </DropdownMenuContent>
    </DropdownMenu>
  );
}
