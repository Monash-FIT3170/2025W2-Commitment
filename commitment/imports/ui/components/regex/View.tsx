import React, { useEffect, useMemo, useState } from "react";
import { useLocation } from "react-router-dom";
import type { DateRange } from "react-day-picker";
import ControlBar from "./ControlBar";
import RulesToolbar from "./RulesToolbar";
import RulesEditor from "./RulesEditor";
import SummaryCards from "./SummaryCards";
import ResultsTable from "./ResultsTable";
import DetailDialog from "./DetailDialog";
import type { DetailCommit, ResultRow } from "./types";
import { compileRulesForClient, parseRulesOrThrow } from "./utils";

type RepoCommit = {
  value: { commitTitle: string; description: string };
};

function isRouteState(s: unknown): s is { repoUrl?: string } {
  return !!s && typeof s === "object" && "repoUrl" in (s as Record<string, unknown>);
}

export default function View(): React.JSX.Element {

  // finding repo, use last opened repo from local storage if can't find
  const location = useLocation();
  const fromState = isRouteState(location.state) ? location.state.repoUrl : undefined;
  const repoUrl: string | null = fromState ?? localStorage.getItem("lastRepoUrl");

  // repo metadata
  const [branches, setBranches] = useState<string[]>([]);
  const [selectedBranch, setSelectedBranch] = useState<string | undefined>(undefined);
  const [contributors, setContributors] = useState<string[]>([]);
  const [selectedContributors, setSelectedContributors] = useState<string[]>([]);
  const [dateRange, setDateRange] = useState<DateRange | undefined>(undefined);

  // set default rules
  const [rulesText, setRulesText] = useState<string>(() =>
    JSON.stringify(
      [
        {
          key: "conventional_header",
          regex: "^(feat|fix|docs|chore|style|refactor|test|ci)(\\(.+\\))?:\\s",
          scale: 2,
          sign: "+",
        },
        { key: "wip_present", regex: "\\bWIP\\b", scale: 1, sign: "-" },
        { key: "merge_message", regex: "^Merge\\spull\\srequest", scale: 1, sign: "-" },
      ],
      null,
      2
    )
  );

  const [helpOpen, setHelpOpen] = useState(false);

  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [results, setResults] = useState<ResultRow[] | null>(null);

  const [detailOpen, setDetailOpen] = useState(false);
  const [detailTitle, setDetailTitle] = useState<string>("");
  const [detailCommits, setDetailCommits] = useState<DetailCommit[]>([]);

  const canRun = useMemo(() => !!repoUrl && !!selectedBranch, [repoUrl, selectedBranch]); // branch + repoURL must be selected 

  // maintain and fetch repo metadata on repo change
  useEffect(() => {
    if (!repoUrl) return;
    localStorage.setItem("lastRepoUrl", repoUrl);

    Meteor.call(
      "repo.getMetadata",
      repoUrl,
      (
        err: Error | null,
        meta:
          | {
              branches: string[];
              contributors: string[];
              dateRange?: { from?: string | Date; to?: string | Date };
            }
          | undefined
      ) => {
        if (err) {
          setError(err.message);
          return;
        }
        const branchList = meta?.branches ?? [];
        setBranches(branchList);

        const [firstBranch] = branchList;
        let preferred: string | undefined;
        if (branchList.includes("main")) preferred = "main";
        else if (branchList.includes("master")) preferred = "master";
        else preferred = firstBranch;
        setSelectedBranch(preferred);

        const cons = meta?.contributors ?? [];
        setContributors(cons);
        setSelectedContributors(cons);

        if (meta?.dateRange?.from && meta?.dateRange?.to) {
          setDateRange({
            from: new Date(meta.dateRange.from),
            to: new Date(meta.dateRange.to),
          });
        }
      }
    );
  }, [repoUrl]);

  // parse and validate regex rules
  const runEvaluation = () => {
    setError(null);
    setResults(null);
    if (!repoUrl || !selectedBranch) return;

    let parsed;
    try {
      parsed = parseRulesOrThrow(rulesText);
    } catch (e: unknown) {
      const message = e instanceof Error ? e.message : "Invalid rules JSON";
      setError(message);
      return;
    }

    setLoading(true);
    Meteor.call(
      "regex.evaluate",
      {
        repoUrl,
        rules: parsed,
        branch: selectedBranch,
        startDate: dateRange?.from,
        endDate: dateRange?.to,
        contributors: selectedContributors,
      },
      (err: Error | null, data: ResultRow[] | undefined) => {
        setLoading(false);
        if (err) {
          setError(err.message);
          return;
        }
        setResults(data ?? []);
      }
    );
  };

  // open detail modal; show contributors and their rules' match hits
  const openContributorDetail = (name: string) => {
    if (!repoUrl || !selectedBranch) return;
    setDetailTitle(name);
    setDetailCommits([]);
    setDetailOpen(true);

    Meteor.call(
      "repo.getFilteredData",
      {
        repoUrl,
        startDate: dateRange?.from ?? new Date(0),
        endDate: dateRange?.to ?? new Date(),
        branch: selectedBranch,
        contributor: [name],
      },
      (
        err: Error | null,
        data:
          | {
              repositoryData: { allCommits: RepoCommit[] };
            }
          | undefined
      ) => {
        if (err) return;
        let compiled: { key: string; re: RegExp }[] = [];
        try {
          compiled = compileRulesForClient(parseRulesOrThrow(rulesText));
        } catch {
          compiled = [];
        }
        // map commit messages and build text from title + desc
        const commits = (data?.repositoryData.allCommits ?? []).map((c) => {
          const title = c.value.commitTitle ?? "(no title)";
          const text = `${title}\n${c.value.description ?? ""}`; 
          const matchedKeys = compiled
            .map(({ key, re }) => (re.test(text) ? key : null))
            .filter((x): x is string => x !== null);
          return { title, matchedKeys };
        });
        setDetailCommits(commits);
      }
    );
  };

  if (!repoUrl) {
    return <div className="p-6">Open a repository first (Home → Metrics).</div>;
  }

  return (
    <div className="w-full h-full border-t border-git-stroke-primary/40 bg-git-bg-elevated dark:bg-git-bg-primary">
      <div className="px-6 py-4">
        <div className="mb-4 flex items-center justify-between">
          <div>
            <h2 className="text-2xl font-semibold text-foreground">Regex Scaling</h2>
            <p className="text-git-text-secondary">
              Analyze commit message quality with weighted regex rules.
            </p>
          </div>
        </div>

        {/* Controls Row (filters and run button) */}
        <div className="flex flex-wrap items-start justify-between gap-6 mb-4">
          <ControlBar
            branches={branches}
            selectedBranch={selectedBranch}
            onBranchChange={setSelectedBranch}
            dateRange={dateRange}
            onDateRangeChange={setDateRange}
            contributors={contributors}
            selectedContributors={selectedContributors}
            onContributorsChange={setSelectedContributors}
            onRun={runEvaluation}
            loading={loading}
            disabled={!canRun}
          />
          <RulesToolbar
            helpOpen={helpOpen}
            setHelpOpen={setHelpOpen}
            onUploadText={setRulesText}
            onError={setError}
          />
        </div>

        {/* json text area for the rules */}
        <RulesEditor rulesText={rulesText} onChange={setRulesText} />

        {error && <div className="text-red-500 text-sm mb-3">{error}</div>}

        <SummaryCards results={results ?? []} />
        <ResultsTable results={results ?? []} onRowClick={openContributorDetail} />

        <DetailDialog
          open={detailOpen}
          onOpenChange={setDetailOpen}
          title={detailTitle}
          commits={detailCommits}
        />
      </div>
    </div>
  );
}
