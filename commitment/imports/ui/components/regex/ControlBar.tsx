import React from "react";
import { Button } from "@base/button";
import type { DateRange } from "react-day-picker";
import BranchDropdownMenu from "../metrics/BranchDropdownMenu";
import { ContributorDropdownMenu } from "../metrics/ContributorDropdownMenu";
import { DatePicker } from "../metrics/date-range-picker";

// Control bar consists of: Branch, Date, Contributors, Run
type Props = {
  branches: string[];
  selectedBranch: string | undefined; 
  onBranchChange: (v: string) => void;

  dateRange: DateRange | undefined; 
  onDateRangeChange: (v?: DateRange) => void;

  contributors: string[];
  selectedContributors: string[];
  onContributorsChange: (v: string[]) => void;

  onRun: () => void;
  loading: boolean;
  disabled: boolean;
};

export default function ControlBar({
  branches,
  selectedBranch = undefined,
  onBranchChange,
  dateRange = undefined,
  onDateRangeChange,
  contributors,
  selectedContributors,
  onContributorsChange,
  onRun,
  loading,
  disabled,
}: Props): React.JSX.Element {
  return (
    <div className="flex flex-wrap gap-6 mb-4">
      <div className="flex flex-col">
        <span className="text-sm text-git-text-secondary mb-1">Branch</span>
        <BranchDropdownMenu
          branches={branches}
          selected={selectedBranch}
          onChange={onBranchChange}
        />
      </div>

      <div className="flex flex-col">
        <span className="text-sm text-git-text-secondary mb-1">Date Range</span>
        <DatePicker defaultValue={dateRange} onChange={onDateRangeChange} />
      </div>

      <div className="flex flex-col">
        <span className="text-sm text-git-text-secondary mb-1">Contributors</span>
        <ContributorDropdownMenu
          contributors={contributors}
          selected={selectedContributors}
          onChange={onContributorsChange}
        />
      </div>

      <div className="self-end">
        <Button variant="default" onClick={onRun} disabled={disabled || loading}>
          {loading ? "Running..." : "Run"}
        </Button>
      </div>
    </div>
  );
}
