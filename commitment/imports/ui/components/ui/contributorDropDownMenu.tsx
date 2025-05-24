"use client";

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

export function ContributorDropDownMenu({
  contributors,
}: DropdownMenuCheckboxesProps) {
  const [selectedContributors, setContributors] =
    React.useState<string[]>(contributors);

  const [isOpen, setIsOpen] = React.useState(false);

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
    <DropdownMenu open={isOpen} onOpenChange={setIsOpen}>
      <DropdownMenuTrigger asChild>
        <Button
          variant="outline"
          className="w-auto justify-start focus:outline-none focus:ring-0"
        >
          {buttonText()}
        </Button>
      </DropdownMenuTrigger>
      <DropdownMenuContent className="w-[280px] focus:outline-none focus:ring-0">
        <DropdownMenuLabel>Select Contributors</DropdownMenuLabel>
        <DropdownMenuSeparator />
        <ScrollArea className="h-48">
          {contributors.map((contributors) => (
            <DropdownMenuCheckboxItem
              key={contributors}
              checked={selectedContributors.includes(contributors)}
              onCheckedChange={(checked) => {
                if (checked) {
                  setContributors((prev) => [...prev, contributors]);
                } else {
                  setContributors((prev) =>
                    prev.filter((c) => c !== contributors)
                  );
                }
              }}
            >
              {contributors}
            </DropdownMenuCheckboxItem>
          ))}
        </ScrollArea>
      </DropdownMenuContent>
    </DropdownMenu>
  );
}
