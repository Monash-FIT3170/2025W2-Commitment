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
const MIN_SEEN_FRACTION = 0.05; // tiny credit if a stage is seen but totals unknown
const clamp01 = (x: number) => Math.max(0, Math.min(1, x));

type FractionResolver = (m: ProgressModel) => number;

const stageFraction: Record<StageKey, FractionResolver> = {
  validate:        (m) => (m.seen.validate ? 1 : 0),
  parseSetup:      (m) => (m.seen.parseSetup ? 1 : 0),
  clone:           (m) => (m.seen.clone ? 1 : 0),
  branches:        (m) => (m.seen.branches ? 1 : 0),
  commitsDiscover: (m) => (m.seen.commitsDiscover ? 1 : 0),

  commitData:      (m) => {
    if (m.commitTotals && m.commitTotals.total > 0) {
      return clamp01(m.commitTotals.done / m.commitTotals.total);
    }
    return m.seen.commitData ? MIN_SEEN_FRACTION : 0;
  },

  filesDiscover:   (m) => (m.seen.filesDiscover ? 1 : 0),

  fileData:        (m) => {
    if (m.fileTotals && m.fileTotals.total > 0) {
      return clamp01(m.fileTotals.done / m.fileTotals.total);
    }
    return m.seen.fileData ? MIN_SEEN_FRACTION : 0;
  },

  finalize:        (m) => (m.seen.finalize ? 1 : 0),
};

/** Compute a target percentage from the model using weights (pure, no +=) */
function computeTargetPercent(model: ProgressModel): number {
  const weightedSum = (Object.keys(STAGE_WEIGHTS) as StageKey[]).reduce((acc, key) => {
    const w = STAGE_WEIGHTS[key];        // weight in [0..1]
    const f = stageFraction[key](model); // fraction in [0..1]
    return acc + w * f;
  }, 0);

  // Convert to percent and cap (avoid hitting full 100% before success)
  return clamp(weightedSum * 100, 0, 98);
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

/** Navigate back if there's a previous entry; otherwise fallback to /home */
function goBackOrHome(navigate: ReturnType<typeof useNavigate>) {
  try {
    // In browsers, history.length > 1 usually means we can go back safely.
    if (typeof window !== "undefined" && window.history && window.history.length > 1) {
      navigate(-1);
      return;
    }
  } catch {
    // ignore and use fallback
  }
  navigate("/home", { replace: true });
}


const LoadingPage: React.FC = () => {
  const navigate = useNavigate();

  const location = useLocation();
  const stateMaybe = (location?.state as LocationState | null) ?? null;
  const repoUrl = stateMaybe?.repoUrl;

  const [model, setModel] = useState<ProgressModel>(createEmptyModel());
  const [message, setMessage] = useState("Starting…");
  const [tipIndex, setTipIndex] = useState(0);
  const [hadError, setHadError] = useState(false);

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
    !model.seen.clone &&
    !model.seen.commitsDiscover &&
    !model.seen.commitData &&
    !model.seen.fileData;

  useEffect(() => {
    if (!repoUrl) return;

    let isMounted = true;
    const notifier = new Subject<string>();
    
    const sub = notifier.subscribe((msg) => {
    if (!isMounted) return;
    setMessage(msg);

    // Functional state update: derive next model from prev, and compute next target from next model
    setModel((prev) => {
      const next = updateModelFromMessage(msg, prev);
      const nextTarget = computeTargetPercent(next);
      setTargetPct(nextTarget);
      return next;
    });
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
          goBackOrHome(navigate);
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
    <div className="min-h-screen flex items-center justify-center px-6 pt-24">
      <div className="w-full max-w-2xl flex flex-col items-center gap-6 bg-git-bg-elevated/0">
        <h2
          className={[
            "text-git-text-primary",
            "text-2xl md:text-3xl font-mono font-semibold text-center",
            hadError ? "text-red-500" : "",
          ].join(" ")}
        >
          {message}
        </h2>

        <div className="w-full">
          <LoadingBar progress={progress} indeterminate={indeterminate} />
        </div>

        <TipBox tip={tips[tipIndex]} />
      </div>
    </div>
  );
};

export default LoadingPage;
