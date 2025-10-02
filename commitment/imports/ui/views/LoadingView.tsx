import React, { useEffect, useMemo, useRef, useState } from "react";
import { Meteor } from "meteor/meteor";
import { useLocation, useNavigate, Navigate } from "react-router-dom";
import { Subject } from "rxjs";

import LoadingBar from "/imports/ui/components/loading/LoadingBar";
import TipBox from "/imports/ui/components/loading/TipBox";
import { fetchRepo } from "@api/call_repo";

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

/** Stage weights (sum ≈ 1.0). No backend changes needed. */
const STAGE_WEIGHTS = {
  validate: 0.05,
  parseSetup: 0.03,
  clone: 0.1,
  branches: 0.05,
  commitsDiscover: 0.1,
  commitData: 0.45,
  filesDiscover: 0.07,
  fileData: 0.12,
  finalize: 0.03,
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
  const model: ProgressModel = { ...prev, seen: { ...prev.seen } };

  if (/Validating repo exists/i.test(m)) model.seen.validate = true;
  if (/Found the repo/i.test(m)) model.seen.validate = true;
  if (/Formulating parsers/i.test(m)) model.seen.parseSetup = true;
  if (/Cloning repo/i.test(m)) model.seen.clone = true;
  if (/Searching for branch names/i.test(m)) model.seen.branches = true;
  if (/Searching for commit hashes/i.test(m)) model.seen.commitsDiscover = true;

  // Commit data progress
  {
    const match = m.match(/Formulating all commit data\s*\((\d+)\s*\/\s*(\d+)\)/i);
    if (match) {
      const done = Number(match[1]);
      const total = Number(match[2]);
      if (Number.isFinite(done) && Number.isFinite(total) && total > 0) {
        model.commitTotals = { done, total };
        model.seen.commitData = true;
      }
    }
  }

  // Files discovery
  {
    const match = m.match(/Found\s+(\d+)\s+distinct file versions/i);
    if (match) {
      const total = Number(match[1]);
      if (Number.isFinite(total) && total > 0) {
        model.filesTotalKnown = true;
        if (!model.fileTotals) {
          model.fileTotals = { done: 0, total };
        } else {
          model.fileTotals = { done: model.fileTotals.done, total };
        }
        model.seen.filesDiscover = true;
      }
    }
  }

 
  {
    const match = m.match(/Formulating all file data\s*\((\d+)\s*\/\s*(\d+)\)/i);
    if (match) {
      const done = Number(match[1]);
      const total = Number(match[2]);
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

  return clamp(percent, 0, 98);
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

  const location = useLocation();
  const stateMaybe = (location?.state as LocationState | null) ?? null;
  const repoUrl = stateMaybe?.repoUrl;

  const [message, setMessage] = useState("Starting…");
  const [tipIndex, setTipIndex] = useState(0);
  const [hadError, setHadError] = useState(false);

  const modelRef = useRef<ProgressModel>(createEmptyModel());
  const [targetPct, setTargetPct] = useState(0);

  const tips = useMemo(
    () => [
      "Tip 1: Use keyboard shortcuts to improve productivity.",
      "Tip 2: Frequently commit your code to avoid loss.",
      "Tip 3: Write meaningful commit messages.",
    ],
    []
  );

  useEffect(() => {
    if (hadError) return;
    const id = Meteor.setInterval(() => {
      setTipIndex((i) => (i + 1) % tips.length);
    }, 4000);
    return () => Meteor.clearInterval(id);
  }, [tips.length, hadError]);

  const progress = useSmoothedProgress(targetPct);

  const indeterminate =
    targetPct < 1 &&
    !modelRef.current.seen.clone &&
    !modelRef.current.seen.commitsDiscover &&
    !modelRef.current.seen.commitData &&
    !modelRef.current.seen.fileData;

  useEffect(() => {
    if (!repoUrl) return;

    let isMounted = true;
    const notifier = new Subject<string>();
    const sub = notifier.subscribe((msg) => {
      if (!isMounted) return;
      setMessage(msg);

      modelRef.current = updateModelFromMessage(msg, modelRef.current);
      const nextTarget = computeTargetPercent(modelRef.current);
      setTargetPct(nextTarget);
    });

    fetchRepo(repoUrl, notifier)
      .then((value: boolean) => {
        if (value === false) throw new Error("fetchRepo failed.");
        notifier.next("Repository data loaded!");
        setTargetPct(100);
        setTimeout(() => {
          navigate("/metrics", { replace: true, state: { repoUrl } });
        }, 700);
      })
      .catch((err) => {
        const errMsg = toUserMessage(err);
        setHadError(true);
        notifier.next(`Error: ${errMsg}`);
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

  return !repoUrl ? (
    <Navigate to="/insert-git-repo" replace />
  ) : (
    <div className="flex flex-col items-center justify-center h-screen pt-24 px-6">
      <h2 className={`text-3xl font-inconsolata-bold mb-6 ${hadError ? "text-red-500" : ""}`}>
        {message}
      </h2>

      <LoadingBar progress={progress} indeterminate={indeterminate} />

      <TipBox tip={tips[tipIndex]} />
    </div>
  );
};

export default LoadingPage;
