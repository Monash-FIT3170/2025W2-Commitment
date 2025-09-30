"use client";

import React, { useState } from "react";
import { ReloadIcon } from "@radix-ui/react-icons";
import { Spinner } from "@ui/components/ui/spinner";

type RefreshButtonProps = {
  url: string;
};

const RefreshButton: React.FC<RefreshButtonProps> = ({ url }) => {
  const [loading, setLoading] = useState(false);

  const handleClick = async () => {
    setLoading(true);

    try {
      const update = await Meteor.call("repoCollection.isUpToDate", url);
      console.log("Refreshed: ", update);
    } catch (err) {
      console.error("Failed to refresh repo:", err);
    } finally {
      setTimeout(() => {
        setLoading(false);
      }, 1500);
    }
  };

  return (
    <button
      onClick={handleClick}
      disabled={loading}
      className="inline-flex items-center justify-center px-3 py-2 text-[#F1502F] transition"
    >
      {loading ? (
        <Spinner
          className="w-4 h-4 animate-spin text-[#F1502F]"
          variant="circle"
        />
      ) : (
        <ReloadIcon className="w-4 h-4" />
      )}
    </button>
  );
};

export default RefreshButton;
