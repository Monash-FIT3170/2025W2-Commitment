import React, { useMemo, useState } from "react";
import { Button } from "@base/button";
import BranchDropdownMenu from "../metrics/BranchDropdownMenu";
import { ContributorDropdownMenu } from "../metrics/ContributorDropdownMenu";

type Props = {
  repoUrl: string;
  branches: string[];
  contributors: string[];
};

type SummaryResponse = {
  repoUrl: string;
  branchName: string;
  contributorName: string;
  count: number;
  commits: { hash: string; title: string; timestamp: string }[];
  summary: string;
};

export default function AiSummarySection({
  repoUrl,
  branches,
  contributors,
}: Props): React.JSX.Element {
  // defaults (prefer main/master if available)
  const defaultBranch = useMemo(() => {
    if (branches.includes("main")) return "main";
    if (branches.includes("master")) return "master";
    return branches[0];
  }, [branches]);

  const [branch, setBranch] = useState<string | undefined>(defaultBranch);
  const [contributor, setContributor] = useState<string | undefined>(contributors[0]);
  const [limit, setLimit] = useState<number>(20);

  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [result, setResult] = useState<SummaryResponse | null>(null);

  const canSummarise = !!repoUrl && !!branch && !!contributor;

  const onSummarise = () => {
    setError(null);
    setResult(null);
    if (!canSummarise) return;

    setLoading(true);
    Meteor.call(
      "ai.summarizeUserCommits",
      {
        repoUrl,
        branchName: branch,
        contributorName: contributor,
        limit,
      },
      (err: Error | null, data: SummaryResponse | undefined) => {
        setLoading(false);
        if (err) {
          setError(err.message);
          return;
        }
        setResult(data ?? null);
      }
    );
  };

  return (
    <div className="mt-10">
      <div className="mb-4 flex items-center justify-between">
        <div>
          <h2 className="text-2xl font-semibold text-foreground">AI Commit Message Summarisation</h2>
          <p className="text-git-text-secondary">
            Select a branch and contributor, then summarise their most recent commit messages.
          </p>
        </div>
      </div>

      {/* Controls */}
      <div className="flex flex-wrap gap-6 mb-4">
        <div className="flex flex-col">
          <span className="text-sm text-git-text-secondary mb-1">Branch</span>
          <BranchDropdownMenu
            branches={branches}
            selected={branch}
            onChange={setBranch}
          />
        </div>

        <div className="flex flex-col">
          <span className="text-sm text-git-text-secondary mb-1">Contributor</span>
          {/* The dropdown returns string[] in your regex section; we accept first item */}
          <ContributorDropdownMenu
            contributors={contributors}
            selected={contributor ? [contributor] : []}
            onChange={(arr) => setContributor(arr[0])}
          />
        </div>

        <div className="flex flex-col">
          <span className="text-sm text-git-text-secondary mb-1">Max Commits</span>
          <input
            type="number"
            min={1}
            max={50}
            value={limit}
            onChange={(e) => setLimit(Math.max(1, Math.min(50, Number(e.target.value) || 1)))}
            className="w-28 bg-transparent border border-git-stroke-primary/50 rounded-md px-3 py-2 text-sm"
          />
        </div>

        <div className="self-end">
          <Button variant="default" onClick={onSummarise} disabled={!canSummarise || loading}>
            {loading ? "Summarising..." : "Summarise"}
          </Button>
        </div>
      </div>

      {error && <div className="text-red-500 text-sm mb-3">{error}</div>}

      {/* Results */}
      <div className="rounded-xl border border-git-stroke-primary/40 bg-git-bg-elevated p-4">
        {!result ? (
          <div className="text-sm text-git-text-secondary">
            No summary yet — choose a branch & contributor, then click <b>Summarise</b>.
          </div>
        ) : (
          <>
            <div className="mb-3">
              <div className="text-sm text-git-text-secondary">
                <span className="mr-3">
                  <b>Branch:</b> {result.branchName}
                </span>
                <span className="mr-3">
                  <b>Contributor:</b> {result.contributorName}
                </span>
                <span>
                  <b>Commits:</b> {result.count}
                </span>
              </div>
            </div>

            <div className="mb-4 whitespace-pre-wrap leading-relaxed">
              {result.summary}
            </div>

            <div>
              <div className="text-sm font-medium mb-2">Commits Included</div>
              {result.commits.length === 0 ? (
                <div className="text-sm text-git-text-secondary">None.</div>
              ) : (
                <ul className="list-disc ml-5">
                  {result.commits.map((c) => (
                    <li key={c.hash} className="mb-1">
                      <span className="font-mono text-xs mr-2">{c.hash.slice(0, 7)}</span>
                      <span className="font-medium">{c.title || "(no title)"}</span>
                      <span className="text-xs text-git-text-secondary ml-2">{c.timestamp}</span>
                    </li>
                  ))}
                </ul>
              )}
            </div>
          </>
        )}
      </div>
    </div>
  );
}
