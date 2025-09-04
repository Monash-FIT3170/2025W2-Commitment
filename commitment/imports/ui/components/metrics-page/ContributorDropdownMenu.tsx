import * as React from "react";
import { Button } from "@ui/components/ui/button";
import {
  DropdownMenu,
  DropdownMenuCheckboxItem,
  DropdownMenuContent,
  DropdownMenuLabel,
  DropdownMenuSeparator,
  DropdownMenuTrigger,
} from "@ui/components/ui/dropdown-menu";

import { ScrollArea } from "@ui/components/ui/scroll-area";

interface DropdownMenuCheckboxesProps {
  contributors: string[];
  selected: string[];
  onChange: (selected: string[]) => void;
}

export function ContributorDropdownMenu({
  contributors,
  selected,
  onChange,
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

  return (
    <DropdownMenu>
      <DropdownMenuTrigger asChild>
        <Button
          variant="outline"
          className={
            'w-[300px] justify-start text-left font-normal border-2 rounded-lg'
           
          }
          style={{ borderColor: '#35353140' }}
        >
          {buttonText()}
        </Button>
      </DropdownMenuTrigger>
      <DropdownMenuContent className="w-[280px] focus:outline-hidden focus:ring-0">
        <DropdownMenuLabel>Select Contributors</DropdownMenuLabel>
        <DropdownMenuSeparator />
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
