import React, { useMemo } from "react";
import type { ResultRow } from "./types";

type Props = { results: ResultRow[] };


export default function SummaryCards({ results }: Props): React.JSX.Element | null {
  
  // mean of scorePerCommit 
  const avgScore = useMemo(() => {
    if (!results || results.length === 0) return 0;
    return results.reduce((acc, r) => acc + r.scorePerCommit, 0) / results.length;
  }, [results]);

  // best suggested scale
  const best = useMemo(() => {
    if (!results || results.length === 0) return null;
    return results.reduce(
      (m, r) => (r.suggestedScale > m.suggestedScale ? r : m),
      results[0]
    );
  }, [results]);

  // worst suggested scale
  const worst = useMemo(() => {
    if (!results || results.length === 0) return null;
    return results.reduce(
      (m, r) => (r.suggestedScale < m.suggestedScale ? r : m),
      results[0]
    );
  }, [results]);

  // regex rule match hits for all contributors
  const ruleTotals = useMemo((): Record<string, number> => {
    const totals: Record<string, number> = {};
    (results ?? []).forEach((r) => {
      Object.entries(r.matchesByRule).forEach(([k, v]) => {
        totals[k] = (totals[k] ?? 0) + v;
      });
    });
    return totals;
  }, [results]);

  if (!results || results.length === 0) return null;

  return (
    <div className="mb-4 grid grid-cols-1 md:grid-cols-4 gap-4">
      <div className="p-3 rounded-lg border border-git-stroke-primary/40">
        <div className="text-sm text-git-text-secondary">Average Score/Commit</div>
        <div className="text-xl font-semibold">{avgScore.toFixed(2)}</div>
      </div>

      <div className="p-3 rounded-lg border border-git-stroke-primary/40">
        <div className="text-sm text-git-text-secondary">Best Scale</div>
        <div className="text-xl font-semibold">
          {best ? `${best.contributor} (${best.suggestedScale.toFixed(2)})` : "-"}
        </div>
      </div>

      <div className="p-3 rounded-lg border border-git-stroke-primary/40">
        <div className="text-sm text-git-text-secondary">Worst Scale</div>
        <div className="text-xl font-semibold">
          {worst ? `${worst.contributor} (${worst.suggestedScale.toFixed(2)})` : "-"}
        </div>
      </div>

      <div className="p-3 rounded-lg border border-git-stroke-primary/40">
        <div className="text-sm text-git-text-secondary">Rule Impact (total matches)</div>
        <div className="mt-1 text-sm">
          {Object.keys(ruleTotals).length === 0
            ? "No matches"
            : Object.entries(ruleTotals).map(([k, v]) => (
                <span key={k} className="inline-block mr-3">
                  {k}: {v}
                </span>
              ))}
        </div>
      </div>
    </div>
  );
}
