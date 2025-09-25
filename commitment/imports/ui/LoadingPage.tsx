import React, { useEffect, useMemo, useRef, useState } from "react";
import { Meteor } from "meteor/meteor";
import { useLocation, useNavigate, Navigate } from "react-router-dom";
import { Subject } from "rxjs";

import LoadingBar from "@ui/components/Loading-page/LoadingBar";
import TipBox from "@ui/components/Loading-page/TipBox";
import { fetchRepo } from "../api/call_repo";

interface LocationState {
  repoUrl?: string;
}

/** Normalize errors at UI boundary */
function toUserMessage(err: unknown): string {
  if (err instanceof Error) return err.message || "Unexpected error occurred.";
  if (typeof err === "string") return err;
  try {
    return JSON.stringify(err);
  } catch {
    return "Unexpected error occurred.";
  }
}


function useServerCrashed(): boolean {
  const [serverCrashed, setServerCrashed] = useState<boolean>(false);

  useEffect(() => {
    const comp = Tracker.autorun(() => {
      const status = Meteor.status().status; // 'connected', 'connecting', 'waiting', 'offline'
      if (status === "offline" || status === "waiting") {
        setServerCrashed(true);
      } else {
        setServerCrashed(false);
      }
    });
    return () => comp.stop();
  }, []);

  return serverCrashed;
}

/** Stage weights (sum ≈ 1.0). No backend changes needed. */
const STAGE_WEIGHTS = {
  validate: 0.05,        // "Validating repo exists…", "Found the repo!"
  parseSetup: 0.03,      // "Formulating parsers..."
  clone: 0.10,           // "Cloning repo..."
  branches: 0.05,        // "Searching for branch names..."
  commitsDiscover: 0.10, // "Searching for commit hashes..."
  commitData: 0.45,      // "Formulating all commit data (d/t)..."
  filesDiscover: 0.07,   // "Found N distinct file versions..."
  fileData: 0.12,        // "Formulating all file data (d/t)..."
  finalize: 0.03         // joining, contributors, repo name, etc.
} as const;

type StageKey = keyof typeof STAGE_WEIGHTS;

type ProgressModel = {
  seen: Partial<Record<StageKey, boolean>>;
  commitTotals?: { done: number; total: number };
  fileTotals?: { done: number; total: number };
  filesTotalKnown?: boolean;
  inFinalize?: boolean;
};

const clamp = (v: number, min = 0, max = 100) => Math.max(min, Math.min(max, v));
const createEmptyModel = (): ProgressModel => ({ seen: {} });

/** Parse a single notifier message into our progress model */
function updateModelFromMessage(m: string, prev: ProgressModel): ProgressModel {
  const model = { ...prev, seen: { ...prev.seen } };

  if (/Validating repo exists/i.test(m)) model.seen.validate = true;
  if (/Found the repo/i.test(m)) model.seen.validate = true;
  if (/Formulating parsers/i.test(m)) model.seen.parseSetup = true;
  if (/Cloning repo/i.test(m)) model.seen.clone = true;
  if (/Searching for branch names/i.test(m)) model.seen.branches = true;
  if (/Searching for commit hashes/i.test(m)) model.seen.commitsDiscover = true;

  // "Formulating all commit data (23/240)..."
  {
    const match = m.match(/Formulating all commit data\s*\((\d+)\s*\/\s*(\d+)\)/i);
    if (match) {
      const done = Number(match[1]), total = Number(match[2]);
      if (Number.isFinite(done) && Number.isFinite(total) && total > 0) {
        model.commitTotals = { done, total };
        model.seen.commitData = true;
      }
    }
  }

  // "Found 123 distinct file versions across all commits"
  {
    const match = m.match(/Found\s+(\d+)\s+distinct file versions/i);
    if (match) {
      const total = Number(match[1]);
      if (Number.isFinite(total) && total > 0) {
        model.filesTotalKnown = true;
        if (!model.fileTotals) model.fileTotals = { done: 0, total };
        else model.fileTotals = { done: model.fileTotals.done, total };
        model.seen.filesDiscover = true;
      }
    }
  }

  // "Formulating all file data (5/123)..."
  {
    const match = m.match(/Formulating all file data\s*\((\d+)\s*\/\s*(\d+)\)/i);
    if (match) {
      const done = Number(match[1]), total = Number(match[2]);
      if (Number.isFinite(done) && Number.isFinite(total) && total > 0) {
        model.fileTotals = { done, total };
        model.filesTotalKnown = true;
        model.seen.fileData = true;
      }
    }
  }

  if (
    /Joining found files with commit data|Formulating all contributors|Linking branches to their commits|Formulating all branch data|Fetching repo name|Data processed/i.test(
      m
    )
  ) {
    model.inFinalize = true;
    model.seen.finalize = true;
  }

  return model;
}

/** Compute a target percentage from the model using weights */
function computeTargetPercent(model: ProgressModel): number {
  let percent = 0;

  (Object.keys(STAGE_WEIGHTS) as StageKey[]).forEach((k) => {
    if (k === "commitData" || k === "fileData" || k === "filesDiscover") return;
    if (model.seen[k]) percent += STAGE_WEIGHTS[k] * 100;
  });

  if (model.commitTotals && model.commitTotals.total > 0) {
    const { done, total } = model.commitTotals;
    percent += STAGE_WEIGHTS.commitData * clamp((done / total) * 100);
  } else if (model.seen.commitData) {
    percent += STAGE_WEIGHTS.commitData * 100 * 0.05;
  }

  if (model.seen.filesDiscover) {
    percent += STAGE_WEIGHTS.filesDiscover * 100;
  }

  if (model.fileTotals && model.fileTotals.total > 0) {
    const { done, total } = model.fileTotals;
    percent += STAGE_WEIGHTS.fileData * clamp((done / total) * 100);
  } else if (model.seen.fileData) {
    percent += STAGE_WEIGHTS.fileData * 100 * 0.05;
  }

  return clamp(percent, 0, 98); // cap until success
}

/** Smoothly ease actual progress toward target using rAF */
function useSmoothedProgress(target: number) {
  const [progress, setProgress] = useState(0);
  const rafRef = useRef<number | null>(null);
  const currentRef = useRef(0);

  useEffect(() => {
    const animate = () => {
      const cur = currentRef.current;
      const diff = target - cur;
      if (Math.abs(diff) < 0.15) {
        currentRef.current = target;
        setProgress(target);
        rafRef.current = null;
        return;
      }
      const step = Math.sign(diff) * Math.max(0.25, Math.min(2.5, Math.abs(diff) * 0.15));
      const next = clamp(cur + step);
      currentRef.current = next;
      setProgress(next);
      rafRef.current = requestAnimationFrame(animate);
    };

    if (rafRef.current == null) {
      rafRef.current = requestAnimationFrame(animate);
    }
    return () => {
      if (rafRef.current != null) cancelAnimationFrame(rafRef.current);
      rafRef.current = null;
    };
  }, [target]);

  return progress;
}


const LoadingPage: React.FC = () => {
  const navigate = useNavigate();
  const { state } = useLocation();
  const { repoUrl } = (state as LocationState) || {};

  const [message, setMessage] = useState<string>("Starting…");
  const [tipIndex, setTipIndex] = useState<number>(0);
  const [hadError, setHadError] = useState<boolean>(false);

  // model derived from notifier text (no backend change)
  const modelRef = useRef<ProgressModel>(createEmptyModel());
  const [targetPct, setTargetPct] = useState<number>(0);

  const serverCrashed = useServerCrashed();


  const tips = useMemo(
    () => [
      "Tip 1: Use keyboard shortcuts to improve productivity.",
      "Tip 2: Frequently commit your code to avoid loss.",
      "Tip 3: Write meaningful commit messages.",
    ],
    []
  );

  // Rotate tips every 4s (pause on error)
  useEffect(() => {
    if (hadError) return;
    const id = Meteor.setInterval(() => {
      setTipIndex((i) => (i + 1) % tips.length);
    }, 4000);
    return () => Meteor.clearInterval(id);
  }, [tips.length, hadError]);

  // If no repo URL provided, redirect immediately
  if (!repoUrl) return <Navigate to="/insert-git-repo" replace />;

  // Smooth progress follows our computed target
  const progress = useSmoothedProgress(targetPct);

  // Early indeterminate state before any meaningful totals show up
  const indeterminate =
    targetPct < 1 &&
    !modelRef.current.seen.clone &&
    !modelRef.current.seen.commitsDiscover &&
    !modelRef.current.seen.commitData &&
    !modelRef.current.seen.fileData;

  // Fetch repository data and stream notifier
  useEffect(() => {
    if (!repoUrl) return;

    let isMounted = true;
    const notifier = new Subject<string>();
    const sub = notifier.subscribe((msg) => {
      if (!isMounted) return;
      setMessage(msg);

      // Update progress model from incoming text
      modelRef.current = updateModelFromMessage(msg, modelRef.current);
      const nextTarget = computeTargetPercent(modelRef.current);
      setTargetPct(nextTarget);
    });

    fetchRepo(repoUrl, notifier)
      .then((value: boolean) => {
        if (value === false) throw new Error("fetchRepo failed.");
        notifier.next("Repository data loaded!");
        setTargetPct(100); // final smooth rise to 100
        setTimeout(() => {
          navigate("/metrics", { replace: true, state: { repoUrl } });
        }, 700);
      })
      .catch((err) => {
        const human = toUserMessage(err);
        setHadError(true);
        // Always push a string into the notifier
        notifier.next(`Error: ${human}`);
        // Humane delay then redirect
        setTimeout(() => {
          navigate("/home", { replace: true });
        }, 8000);
      })
      .finally(() => {
        notifier.complete();
      });

    return () => {
      isMounted = false;
      sub.unsubscribe();
      notifier.complete();
    };
  }, [repoUrl, navigate]);

  return (
    <>
      <div className="fixed top-0 w-full z-10"></div>
      <div className="flex flex-col items-center justify-center h-screen pt-24 px-6">
        {/* Live notifier message */}
        <h2 className={`text-3xl font-inconsolata-bold mb-6 ${hadError ? "text-red-500" : ""}`}>
          {message}
        </h2>

        <LoadingBar progress={progress} indeterminate={indeterminate} />

        {/* Tip box remains as-is (tips only) */}
        <TipBox tip={tips[tipIndex]} />
      </div>
    </>
  );
};


export default LoadingPage;
