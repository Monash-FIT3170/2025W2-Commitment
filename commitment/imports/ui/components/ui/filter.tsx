import {
  DropdownMenu,
  DropdownMenuTrigger,
  DropdownMenuContent,
  DropdownMenuItem,
} from "@ui/components/ui/dropdown";
import React from "react";
import { Button } from "./button";
import { Filter as FilterIcon } from "lucide-react";

// inputs, filter option +type of filter option
// onchange method 

export default function Filter() {
  return (
    <DropdownMenu>
      <DropdownMenuTrigger asChild>
      <Button variant="outline" size="icon">
    <FilterIcon />
  </Button>
      </DropdownMenuTrigger>
      <DropdownMenuContent>
        {/* allow for user to put in */}
        <DropdownMenuItem>Profile</DropdownMenuItem>
        <DropdownMenuItem>Billing</DropdownMenuItem>
        <DropdownMenuItem>Team</DropdownMenuItem>
        <DropdownMenuItem>Subscription</DropdownMenuItem>
      </DropdownMenuContent>
    </DropdownMenu>
  );
}


