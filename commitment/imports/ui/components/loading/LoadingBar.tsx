import React, { FC } from "react";

export interface LoadingBarProps {
  progress: number;
  indeterminate?: boolean;
}

const clamp = (v: number, min = 0, max = 100) => Math.max(min, Math.min(max, v));

const LoadingBar: FC<LoadingBarProps> = ({ progress, indeterminate = false }) => {
  const pct = clamp(progress);

  return (
    <div
      className={[
        "w-full h-3 md:h-4 rounded-full overflow-hidden",
        "bg-git-bg-secondary border border-git-stroke-secondary",
        "relative",
      ].join(" ")}
    >
      {/* Determinate fill */}
      <div
        className={[
          "h-full transition-[width] duration-150 ease-out",
          "bg-git-int-primary",
        ].join(" ")}
        style={{ width: indeterminate ? "0%" : `${pct}%` }}
      />
    </div>
  );
};

export default LoadingBar;
