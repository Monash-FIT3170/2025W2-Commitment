import React, { ReactNode } from "react";

interface GraphCardProps {
  children: ReactNode;
  className?: string
}

const GraphCard: React.FC<GraphCardProps> = ({
  children, className = ""
}) => {
  return (
    <div
      className={`outline-solid outline-2 rounded-2xl p-4 basis-1/3 ${className}`}
      style={{
        outlineColor: "#35353140",
        backgroundColor: "#E8E8DD"
      }}
    >
      {children}
    </div>
  );
};

export default GraphCard;