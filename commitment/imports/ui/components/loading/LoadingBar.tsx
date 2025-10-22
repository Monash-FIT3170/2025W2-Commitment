import React, { FC } from "react";

export interface LoadingBarProps {
  progress: number;
  /** when true, show a subtle indeterminate animation */
  indeterminate?: boolean;
}

const LoadingBar: FC<LoadingBarProps> = ({ progress, indeterminate = false }) => {
  const pct = Math.max(0, Math.min(100, progress));
  return (
    <div className="w-full max-w-xl h-4 bg-gray-700 rounded-full overflow-hidden relative">
      {/* Determinate fill */}
      <div
        className="h-full transition-all duration-150"
        style={{
          width: indeterminate ? "0%" : `${pct}%`,
          backgroundColor: indeterminate ? "transparent" : "#fa9f2a",
        }}
      />
      {/* Indeterminate shimmer */}
      {indeterminate && (
        <div
          className="absolute inset-y-0 h-full"
          style={{
            left: 0,
            width: "30%",
            background:
              "linear-gradient(90deg, rgba(250,159,42,0) 0%, rgba(250,159,42,0.8) 50%, rgba(250,159,42,0) 100%)",
            animation: "lb-shimmer 1.1s infinite",
          }}
        />
      )}
      <style>{`
        @keyframes lb-shimmer {
          0% { transform: translateX(-30%); }
          100% { transform: translateX(330%); }
        }
      `}</style>
    </div>
  );
};

export default LoadingBar;
