import React from "react";
import { Button } from "@base/button";
import { Dialog, DialogContent } from "@base/dialog";

type DetailCommit = {
  title: string;
  matchedKeys: string[];
};

type Props = {
  open: boolean;
  onOpenChange: (v: boolean) => void;
  title: string;
  commits: DetailCommit[];
};

// Modal showing single contributor's commits and regex rules matched
export default function DetailDialog({
  open,
  onOpenChange,
  title,
  commits,
}: Props): React.JSX.Element {
  return (
    <Dialog open={open} onOpenChange={onOpenChange}>
      <DialogContent>
        <div className="text-lg font-semibold mb-2">{title} — Matching Commits</div>
        <div className="max-h-[420px] overflow-y-auto">
          {commits.length === 0 ? (
            <div className="text-git-text-secondary text-sm">
              No commits in this selection.
            </div>
          ) : (
            <ul className="list-disc ml-5">
              {commits.map((c) => {
                const key = `${c.title}|${c.matchedKeys.join(",")}`;
                return (
                  <li key={key} className="mb-2">
                    <div className="font-medium">{c.title}</div>
                    <div className="text-xs text-git-text-secondary">
                      {c.matchedKeys.length === 0
                        ? "No matched rules"
                        : c.matchedKeys.map((k) => (
                            <span key={k} className="inline-block mr-2">
                              {k}
                            </span>
                          ))}
                    </div>
                  </li>
                );
              })}
            </ul>
          )}
        </div>
        <div className="flex justify-end mt-3">
          <Button variant="outline" onClick={() => onOpenChange(false)}>
            Close
          </Button>
        </div>
      </DialogContent>
    </Dialog>
  );
}
