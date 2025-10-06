import React, { useState } from "react";
import { Subject } from "rxjs";
import { ReloadIcon } from "@radix-ui/react-icons";
import { Spinner } from "@ui/components/base/spinner";
import { useToast } from "../../hooks/useToast";

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
      title: "toast message for reactive update of refreshing repo",
      description: msg,
      variant: "destructive",
    });
  };

  const msgHandler = new Subject<string>();
  const $toastMsg = msgHandler.subscribe(handleToast);

  const handleClick = async () => {
    setLoading(true);

    const updated = new Subject<boolean>();
    const $updated = updated.subscribe((updated: boolean) => {
      // TODO can show that the repo is up to date or not
    });

    updateRepo(url, updated, msgHandler)
      .then((proceed: boolean) => {
        setLoading(false);
        // call to update the metrics here with a callback / notification
        // does not run if it has not updated in the server
        if (proceed && onRefresh) onRefresh(url);
      })
      .catch((_e: Error) => {
        // something went wrong here we dont know
        // ideally we want to emit this message as this is
        // an unawaited promise so
        // no error will be propagated forward
        // so we need to let them know an erorr occured
        // TODO not by me :D
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
