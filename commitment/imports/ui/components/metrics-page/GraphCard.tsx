import React, { ReactNode } from "react";

interface GraphCardProps {
  children: ReactNode;
  className?: string;
}

function GraphCard({ children, className = "" }: GraphCardProps) {
  return (
    <div
      className={`outline-solid outline-2 outline-git-bg-secondary rounded-2xl p-4 basis-1/3 bg-git-bg-bottom max-w-[486px] min-w-[486px] flex flex-col ${className}`}
    >
      {children}
    </div>
  );
}

export default GraphCard;
