"use client";

import React, { useState } from "react";
import { ReloadIcon } from "@radix-ui/react-icons";
import { Spinner } from "@ui/components/base/spinner";

type RefreshButtonProps = {
  url: string;
  onRefresh?: (url: string) => void;
};

const RefreshButton: React.FC<RefreshButtonProps> = ({ url, onRefresh }) => {
  const [loading, setLoading] = useState(false);

  const handleClick = async () => {
    setLoading(true);

    try {
      const update = await Meteor.callAsync("repoCollection.isUpToDate", url);
      console.log("Refreshed: ", update);
      if (onRefresh) {
        console.log("refreshing");
        onRefresh(url);
      }
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
