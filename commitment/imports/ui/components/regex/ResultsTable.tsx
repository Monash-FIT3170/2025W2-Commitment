import React from "react";
import type { ResultRow } from "./types";

type Props = {
  results: ResultRow[];
  onRowClick: (contributor: string) => void;
};


// Table showing results 
export default function ResultsTable({ results, onRowClick }: Props): React.JSX.Element {
  return (
    <div className="border rounded-lg border-git-stroke-primary/40 overflow-hidden">
      <div className="max-h-[420px] overflow-y-auto">
        <table className="w-full text-sm">
          <thead className="sticky top-0 bg-git-bg-elevated">
            <tr>
              <th className="text-left p-3 border-b border-git-stroke-primary/40">Contributor</th>
              <th className="text-left p-3 border-b border-git-stroke-primary/40">Commits</th>
              <th className="text-left p-3 border-b border-git-stroke-primary/40">Score/Commit</th>
              <th className="text-left p-3 border-b border-git-stroke-primary/40">Scale</th>
              <th className="text-left p-3 border-b border-git-stroke-primary/40">Matches per Rule (Click Each Rule for Exact Match)</th>
            </tr>
          </thead>
          <tbody>
            {results.map((r) => (
              <tr
                key={r.contributor}
                className="odd:bg-git-bg-secondary/40 cursor-pointer"
                onClick={() => onRowClick(r.contributor)}
              >
                <td className="p-3">{r.contributor}</td>
                <td className="p-3">{r.commits}</td>
                <td className="p-3">{r.scorePerCommit.toFixed(2)}</td>
                <td className="p-3">{r.suggestedScale.toFixed(2)}</td>
                <td className="p-3">
                  {Object.entries(r.matchesByRule).map(([k, v]) => (
                    <span key={k} className="inline-block mr-3">
                      {k}: {v}
                    </span>
                  ))}
                </td>
              </tr>
            ))}
            {results.length === 0 && (
              <tr>
                <td className="p-3 text-git-text-secondary" colSpan={5}>
                  No results yet. Provide rules and click Run.
                </td>
              </tr>
            )}
          </tbody>
        </table>
      </div>
    </div>
  );
}
