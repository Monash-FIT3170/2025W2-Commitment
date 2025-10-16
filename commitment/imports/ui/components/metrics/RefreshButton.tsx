import React, { useState, useEffect, useRef } from "react";
import { Subject } from "rxjs";
import { ReloadIcon } from "@radix-ui/react-icons";
import { Spinner } from "@ui/components/base/spinner";
import { useToast } from "@hook/useToast";

import { updateRepo } from "@api/call_repo";

type RefreshButtonProps = {
  url: string;
  onRefresh?: (url: string) => void;
};

const RefreshButton: React.FC<RefreshButtonProps> = ({ url, onRefresh }: RefreshButtonProps) => {
  const [loading, setLoading] = useState(false);
  const { toast } = useToast();

  const handleToast = (msg: string) => {
    // update toast msg with a timeout of 1s
    toast({
      title: "Repository Status",
      description: msg,
      variant: "default",
    });
  };

  const msgHandlerRef = useRef(new Subject<string>());
  const updatedRef = useRef(new Subject<boolean>());

  useEffect(() => {
    const toastSub = msgHandlerRef.current.subscribe(handleToast);
    const updatedSub = updatedRef.current.subscribe((updated: boolean) => {
      updated
        ? handleToast("Repo is up to date!")
        : handleToast("Repo is out of sync, updating...");
    });

    return () => {
      toastSub.unsubscribe();
      updatedSub.unsubscribe();
    };
  }, []);

  const handleClick = async () => {
    setLoading(true);

    // const updated = new Subject<boolean>();
    // const $updated = updated.subscribe((updated: boolean) => {
    //   // TODO can show that the repo is up to date or not
    // });

    updateRepo(url, updatedRef.current, msgHandlerRef.current)
      .then((proceed: boolean) => {
        setLoading(false);
        // call to update the metrics here with a callback / notification
        // does not run if it has not updated in the server
        if (proceed && onRefresh) onRefresh(url);
      })
      .catch((_e: Error) => {
        setLoading(false);
        handleToast(`An unexpected error occurred: ${_e.message}`);
      });
  };

  return (
    <button
      onClick={handleClick}
      disabled={loading}
      className="inline-flex items-center justify-center px-3 py-2 text-[#F1502F] transition"
    >
      {loading ? (
        <Spinner className="w-4 h-4 animate-spin text-[#F1502F]" variant="circle" />
      ) : (
        <ReloadIcon className="w-4 h-4" />
      )}
    </button>
  );
};

export default RefreshButton;
