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
}

export function ContributorDropdownMenu({
  contributors,
}: DropdownMenuCheckboxesProps) {
  const [selectedContributors, setContributors] =
    React.useState<string[]>(contributors);

  const allSelected =
    selectedContributors.length === contributors.length &&
    contributors.length > 0;

  const maxDisplayCount = 5;

  const buttonText = () => {
    if (allSelected) {
      return "All Contributors";
    }
    if (selectedContributors.length === 0) {
      return "Select Contributors";
    }
    if (selectedContributors.length > maxDisplayCount) {
      const displayed = selectedContributors
        .slice(0, maxDisplayCount)
        .join(", ");
      return `${displayed}, ...`;
    }
    return selectedContributors.join(", ");
  };

  return (
    <DropdownMenu>
      <DropdownMenuTrigger asChild>
        <Button
          variant="outline"
          className="w-[280px] border-2 focus:outline-none focus:ring-0 justify-start px-3"
        >
          <span className="block w-full truncate text-left">
            {buttonText()}
          </span>
        </Button>
      </DropdownMenuTrigger>
      <DropdownMenuContent className="w-[280px] focus:outline-none focus:ring-0">
        <DropdownMenuLabel>Select Contributors</DropdownMenuLabel>
        <DropdownMenuSeparator />
        <ScrollArea className="h-48">
          {contributors.map((contributor) => (
            <DropdownMenuCheckboxItem
              onSelect={(event) => event.preventDefault()}
              key={contributor}
              checked={selectedContributors.includes(contributor)}
              onCheckedChange={(checked) => {
                if (checked) {
                  setContributors((prev) => [...prev, contributor]);
                } else {
                  setContributors((prev) =>
                    prev.filter((c) => c !== contributor)
                  );
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
