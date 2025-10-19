import { SerializableRepoData, CommitData } from "@api/types";
import { getCommitsFrom, getContributors } from "./helper_functions";

// the structure of one user inputted regex rule
export type RegexRule = {
  pattern: string; // regex rule
  weight: number; // scaling factor (relative importance of this rule: higher --> more important) only a magnitude
  negate?: boolean; // if true then the scaling factor is reversed; a penalty (i.e regex rule to catch bad behaviour)
  flags?: string; // regex flags, g and y (global/sticky) not allowed as they stateful (only using one regex instance for all commits)
  key?: string; // name for this regex pattern: maybe a good idea to enforce
};

// structure of the output
export type ContributorRegexScore = {
  contributor: string;
  commits: number;
  matchesByRule: Record<string, number>;
  scoreRaw: number;
  scorePerCommit: number;
  suggestedScale: number;
};

// only inspect first 150 characters
const MAX_FIELD_LEN = 150;


// normalize string 
function trimField(s: string | undefined | null): string {
  if (!s) return "";
  return s.length > MAX_FIELD_LEN ? s.slice(0, MAX_FIELD_LEN) : s;
}

// normalize regex flags, disallow stateful flags (g and y) 
function ensureSafeFlags(base: string | undefined): string {
  const raw = base ?? "i";
  const allowed = ["i", "m", "s", "u"];

  const out = raw
    .split("")
    .filter(Boolean)
    .filter((ch, idx, arr) => allowed.includes(ch) && arr.indexOf(ch) === idx);

  if (out.length === 0) out.push("i");
  return out.join("");
}


function compileRegex(pattern: string, flags?: string): RegExp | null {
  try {
    return new RegExp(pattern, ensureSafeFlags(flags));
  } catch {
    return null;
  }
}

// combine commit message/description 
function commitMessageText(commit: CommitData): string {
  const title = trimField(commit.commitTitle);
  const desc = trimField(commit.description);
  return title && desc ? `${title}\n${desc}` : title || desc;
}

/**
 * Main function: get regex rules from user (in the form of RegexRule above)
 * Compute raw scores, normalize by commute volume (no bonus for higher commits)
 * Find mean/sd for all contributors, apply bucketed scaling
 */
export function evaluateCommitMessageRules(
  repo: SerializableRepoData,
  rulesIn: RegexRule[]
): ContributorRegexScore[] {

  const rules = (rulesIn || []).filter((r) => r && typeof r.pattern === "string");

  const contributors = getContributors(repo);

  // raw sums and score per commit for all contributors
  const rawContributorStats: Array<{
    contributor: string;
    commits: number;
    matchesByRule: Record<string, number>;
    scoreRaw: number;
    scorePerCommit: number;
  }> = contributors.map((name) => {
    const commits: CommitData[] = getCommitsFrom(repo, name);
    const commitsCount = commits.length;

    const matchesByRule: Record<string, number> = {};
    let scoreRaw = 0;

    rules.forEach((rule) => {
      const key = rule.key || rule.pattern; // should maybe enforce/encourage a rule.key input to make presentation more clear if we're to show specific commits on UI
      const re = compileRegex(rule.pattern, rule.flags);

      if (!re) {
        matchesByRule[key] = 0;
        return;
      }

      const ruleSum = commits.reduce((acc, c) => {
        const text = commitMessageText(c);
        return acc + (re.test(text) ? 1 : 0);
      }, 0);

      matchesByRule[key] = ruleSum;

      const signedWeight = (rule.negate ? -1 : 1) * Math.abs(rule.weight || 0);
      scoreRaw += signedWeight * ruleSum;
    });

    const scorePerCommit = commitsCount > 0 ? scoreRaw / commitsCount : 0;

    return {
      contributor: name,
      commits: commitsCount,
      matchesByRule,
      scoreRaw,
      scorePerCommit,
    };
  });

  // suggest scaling, similar to scaling function
  const scores = rawContributorStats.map((r) => r.scorePerCommit);
  const n = scores.length;

  const mean = n ? scores.reduce((a, b) => a + b, 0) / n : 0;

  const variance = n
    ? scores.reduce((sum, x) => {
        const d = x - mean;
        return sum + d * d;
      }, 0) / n
    : 0;

  const std = Math.sqrt(variance);
  const safeStd = std || 1;

  function bucketedScale(diff: number): number {
    if (diff <= -3 * safeStd) return 0.0;
    if (diff <= -2 * safeStd) return 0.5;
    if (diff <= -1 * safeStd) return 0.9;
    if (diff <= 1.2 * safeStd) return 1.0;
    if (diff <= 3 * safeStd) return 1.1;
    return 1.2;
  }

  // add bucketed scale to each contributor and return
  return rawContributorStats.map((r) => {
    const diff = r.scorePerCommit - mean;
    const scale = bucketedScale(diff);
    return {
      contributor: r.contributor,
      commits: r.commits,
      matchesByRule: r.matchesByRule,
      scoreRaw: r.scoreRaw,
      scorePerCommit: r.scorePerCommit,
      suggestedScale: Math.round(scale * 100) / 100,
    };
  });
}
