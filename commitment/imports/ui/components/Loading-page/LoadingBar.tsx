import React, { FC } from "react";

export interface LoadingBarProps {

  progress: number;
}


const LoadingBar: FC<LoadingBarProps> = ({ progress }) => {
  const pct = Math.max(0, Math.min(100, progress));
  return (
    <div className="w-full max-w-xl h-4 bg-gray-700 rounded-full overflow-hidden">
    <div
        className="h-full transition-all duration-200"
        style={{ width: `${pct}%`, backgroundColor: "#fa9f2a" }}
    />
    </div>
  );
};

export default LoadingBar;
