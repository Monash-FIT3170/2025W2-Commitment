import React from "react";
import { Bookmark, Info } from "lucide-react";

export interface RepoRowProps {
  name: string;
  onClick?: () => void;
  onBookmark?: () => void;
  onInfo?: () => void;
  className?: string;
}

const RepoRow = React.forwardRef<HTMLLIElement, RepoRowProps>(
  (
    { name, onClick, onBookmark, onInfo, className = "" },
    ref
  ) => (
    <li
      ref={ref}
      className={`bg-[#F1502F] text-white rounded-xl flex items-center px-4 py-2 gap-3 shadow-sm hover:bg-red-500 transition-colors w-full sm:w-auto ${className}`}
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
      <button
        type="button"
        onClick={onClick}
        className="ml-1 px-4 py-1 rounded-full bg-white text-black text-sm font-semibold hover:bg-gray-200 focus:outline-none flex items-center gap-1 whitespace-nowrap"
      >
        View
      </button>
    </li>
  )
);

RepoRow.displayName = "RepoRow";
export default RepoRow;
