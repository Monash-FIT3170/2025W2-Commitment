import React from "react";
import { Bookmark, Info } from "lucide-react";
import { Button } from "../../ui/button";

export interface RepoRowProps {
  name: string;
  onclick?: () => void;
  onBookmark?: () => void;
  onInfo?: () => void;
}

const RepoRow = React.forwardRef<HTMLLIElement, RepoRowProps>(
  (
    { name, onclick, onBookmark, onInfo },
    ref
  ) => (
    <li
      ref={ref}
      className={`bg-[#F1502F] text-white rounded-xl flex items-center px-4 py-2 gap-3 shadow-sm hover:bg-red-500 transition-colors w-full sm:w-auto `}
    >
      {/* repo name */}
      <span className="flex-1 truncate font-medium">{name}</span>

      {/* info icon */}
      <button
        type="button"
        onClick={onInfo}
        aria-label="repository info"
        className="text-white/80 hover:text-white focus:outline-none"
      >
        <Info size={18} />
      </button>

      {/* bookmark */}
      <button
        type="button"
        onClick={onBookmark}
        aria-label="bookmark repository"
        className="text-white/80 hover:text-white focus:outline-none"
      >
        <Bookmark size={18} />
      </button>

      {/* view */}
      <Button variant="secondary" onClick={onclick}> View Repository</Button>

    </li>
  )
);

RepoRow.displayName = "RepoRow";
export default RepoRow;
