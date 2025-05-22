import React from "react";
import { List, LayoutGrid } from "lucide-react";

interface ViewToggleProps {
  value: "list" | "gallery";
  onChange: (next: "list" | "gallery") => void;
  className?: string;
}

const ViewToggle: React.FC<ViewToggleProps> = ({ value, onChange, className = "" }) => {
  const isList = value === "list";
  const label  = isList ? "List" : "Gallery";

  // sizes chosen so: icon 16px, 4px side padding, knob exactly covers icon
  const PILL_W = "w-20";   // 80px
  const PILL_H = "h-8";    // 32px
  const KNOB   = "w-6 h-6"; // 24px
  const ICON   = 16;
  const OFFSET = "left-1 right-1"; // 4px gap to edge

  return (
    <button
      type="button"
      onClick={() => onChange(isList ? "gallery" : "list")}
      aria-label={`switch to ${isList ? "gallery" : "list"} view`}
      className={`group flex flex-col items-center gap-0.5 ${className}`}
    >
      <div
        className={`relative bg-[#F1502F] rounded-full ${PILL_W} ${PILL_H} flex items-center justify-between px-2 select-none`}
      >
        <List size={ICON} className="text-black z-0" />
        <LayoutGrid size={ICON} className="text-black z-0" />

        <span
          className={`absolute top-1/2 -translate-y-1/2 ${KNOB} bg-white rounded-full shadow-md z-10 transition-all duration-150 ${
            isList ? "right-1" : "left-1"
          }`}
        />
      </div>

      <span className="text-black font-serif italic text-sm leading-none select-none">
        {label}
      </span>
    </button>
  );
};

export default ViewToggle;
