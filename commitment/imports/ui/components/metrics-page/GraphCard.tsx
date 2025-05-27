import React, { ReactNode } from "react";

interface GraphCardProps {
  children: ReactNode;
}

const GraphCard: React.FC<GraphCardProps> = ({
  children,
}) => {
  return (
    <div
      className={`outline outline-2 rounded-2xl p-4 basis-1/3`}
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