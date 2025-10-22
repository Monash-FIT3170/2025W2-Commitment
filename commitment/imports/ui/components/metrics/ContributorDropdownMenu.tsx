import * as React from "react";
import { Button } from "@base/button";
import {
  DropdownMenu,
  DropdownMenuCheckboxItem,
  DropdownMenuContent,
  DropdownMenuLabel,
  DropdownMenuSeparator,
  DropdownMenuTrigger,
} from "@base/dropdown-menu";
import { ChevronDown } from "lucide-react";

import { ScrollArea } from "@base/scroll-area";

interface DropdownMenuCheckboxesProps {
  contributors: string[];
  selected: string[];
  onChange: (selected: string[]) => void;
  allowSelectAll?: boolean;
}

export function ContributorDropdownMenu({
  contributors,
  selected,
  onChange,
  allowSelectAll = true,
}: DropdownMenuCheckboxesProps): React.JSX.Element {
  const allSelected =
    selected.length === contributors.length && contributors.length > 0;

  const maxDisplayCount = 5;

  const buttonText = () => {
    if (allSelected) return "All Contributors";
    if (selected.length === 0) return "Select Contributors";
    if (selected.length > maxDisplayCount) {
      const displayed = selected.slice(0, maxDisplayCount).join(", ");
      return `${displayed}, ...`;
    }
    return selected.join(", ");
  };

  const handleToggleAll = () => {
    if (allSelected) {
      onChange([]);
    } else {
      onChange([...contributors]);
    }
  };

  return (
    <DropdownMenu modal={false}>
      <DropdownMenuTrigger asChild>
        <Button
          variant="outline"
          className="w-[300px] justify-start text-left font-normal border-2 rounded-lg border-git-stroke-primary/40"
        >
          <span className="w-full truncate text-left flex justify-between items-center">
            {buttonText()}
            <ChevronDown className="h-4 w-4 opacity-50" />
          </span>
        </Button>
      </DropdownMenuTrigger>
      <DropdownMenuContent className="w-[280px] focus:outline-none focus:ring-0 border-2 border-git-stroke-primary/40">
        <DropdownMenuLabel>Select Contributors</DropdownMenuLabel>
        <DropdownMenuSeparator />

        {allowSelectAll && (
          <>
            <DropdownMenuCheckboxItem
              checked={allSelected}
              onSelect={(e) => e.preventDefault()}
              onCheckedChange={handleToggleAll}
            >
              {allSelected ? "Unselect All" : "Select All"}
            </DropdownMenuCheckboxItem>

            <DropdownMenuSeparator />
          </>
        )}

        <ScrollArea className="h-48">
          {contributors.map((contributor) => (
            <DropdownMenuCheckboxItem
              onSelect={(event) => event.preventDefault()}
              key={contributor}
              checked={selected.includes(contributor)}
              onCheckedChange={(checked) => {
                if (checked) {
                  onChange([...selected, contributor]);
                } else {
                  onChange(selected.filter((c) => c !== contributor));
                }
              }}
            >
              {contributor}
            </DropdownMenuCheckboxItem>
          ))}
        </ScrollArea>
      </DropdownMenuContent>
    </DropdownMenu>
  );
}
