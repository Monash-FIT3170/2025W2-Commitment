// commitment/server/ai_commit_summary.ts

import * as Meteor from "meteor/meteor";

// ---- Types mirrored from Haskell API README (and Types.hs JSON shape) ----
type ChangeType = "A" | "M" | "D" | "R" | "C";

type FileChanges = Readonly<{
  filepath: string;
  oldFilePath: string;
  char: ChangeType;
  likeness: number;
  newLines: number;
  deletedLines: number;
  diff: string[];
}>;

type CommitData = Readonly<{
  commitHash: string;
  commitTitle: string;
  contributorName: string;
  description: string;
  timestamp: string;
  fileData: FileChanges[];
}>;

type BranchData = Readonly<{
  branchName: string;
  commitHashes: string[];
}>;

type ContributorData = Readonly<{
  name: string;
  emails: string[];
}>;

type RepositoryData = Readonly<{
  repoName: string;
  branches: BranchData[];
  allCommits: Record<string, CommitData>;
  contributors: Record<string, ContributorData>;
}>;

// ---- Config ---- (need to finalise this - just base code so far)
const API_BASE = process.env.HASKELL_API_BASE ?? "http://localhost:8081"; // Haskell HTTP fallback handler
const OPENAI_API_KEY = process.env.OPENAI_API_KEY ?? "";
const OPENAI_MODEL = process.env.OPENAI_MODEL ?? "gpt-4o-mini";

// ---- Small helpers ----
function toArray<T>(obj: Record<string, T>): Array<[string, T]> {
  return Object.entries(obj || {});
}

function getBranchByName(data: RepositoryData, branchName: string): BranchData | null {
  return data.branches.find(b => b.branchName === branchName) ?? null;
}

function parseIso(s: string): number {
  // timestamp is ISO string per Haskell API; convert to millis for sorting
  const t = Date.parse(s);
  return Number.isNaN(t) ? 0 : t;
}

// Fetch RepositoryData from Haskell HTTP API
async function fetchRepositoryData(repoUrl: string): Promise<RepositoryData> {
  const res = await fetch(API_BASE, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ url: repoUrl })
  });

  if (!res.ok) {
    const text = await res.text().catch(() => "");
    throw new Meteor.Error("APIError", `Haskell API status ${res.status}: ${text}`);
  }

  const json = await res.json().catch(() => ({}));
  if (!json || json.type !== "value" || !json.data) {
    throw new Meteor.Error("APIError", "Unexpected Haskell API payload (expected {type:'value', data:...}).");
  }
  return json.data as RepositoryData;
}

// Pick last N commits for (branch, contributor)
function selectLastCommits(
  data: RepositoryData,
  branchName: string,
  contributorName: string,
  limit: number
): CommitData[] {
  const branch = getBranchByName(data, branchName);
  if (!branch) return [];

  // Build an array of commit objects for the given branch
  const commitsOnBranch: CommitData[] = branch.commitHashes
    .map(h => data.allCommits[h])
    .filter((c): c is CommitData => Boolean(c));

  // Filter by contributor and sort newest first by timestamp string
  const byContributor = commitsOnBranch
    .filter(c => c.contributorName === contributorName)
    .sort((a, b) => parseIso(b.timestamp) - parseIso(a.timestamp));

  // Take last N (newest first)
  return byContributor.slice(0, Math.max(1, limit));
}

// Truncate helper to keep prompts sane
function truncate(s: string, max = 280): string {
  if (s.length <= max) return s;
  return s.slice(0, max - 3) + "...";
}

// Build a compact prompt from commits
function buildCommitSummaryPrompt(repoUrl: string, branch: string, user: string, commits: CommitData[]): string {
  const header = `Summarise the following recent commits for contributor "${user}" on branch "${branch}" of repo ${repoUrl}.
Provide 4–6 bullet points capturing: themes, feature areas touched, bugfixes, refactors, and any notable risks or TODOs.
Be concise and objective.
`;

  const lines = commits.map((c, idx) => {
    const title = truncate(c.commitTitle || "(no title)", 160);
    const desc  = truncate(c.description || "", 400);
    const when  = c.timestamp;
    return `#${idx + 1} [${when}] ${c.commitHash.slice(0,7)} — ${title}${desc ? `\n    ${desc}` : ""}`;
  });

  return `${header}\n${lines.join("\n")}`;
}

// Call OpenAI if key provided; otherwise use a tiny heuristic fallback
async function summariseWithAI(prompt: string): Promise<string> {
  if (!OPENAI_API_KEY) {
    // Fallback: naive bullet list of commit titles (keeps the flow working without a key)
    const bullets = prompt
      .split("\n")
      .filter(l => l.startsWith("#"))
      .map(l => l.replace(/^#\d+\s+/, "").trim())
      .slice(0, 6)
      .map(s => `• ${s}`);
    return `No OPENAI_API_KEY set. Heuristic summary:\n${bullets.join("\n")}`;
  }

  // Minimal OpenAI Chat Completions call (fetch-based)
  const res = await fetch("https://api.openai.com/v1/chat/completions", {
    method: "POST",
    headers: {
      Authorization: `Bearer ${OPENAI_API_KEY}`,
      "Content-Type": "application/json"
    },
    body: JSON.stringify({
      model: OPENAI_MODEL,
      messages: [
        { role: "system", content: "You are a helpful, concise software eng. commit summarizer." },
        { role: "user", content: prompt }
      ],
      temperature: 0.2,
      max_tokens: 400
    })
  });

  if (!res.ok) {
    const text = await res.text().catch(() => "");
    throw new Meteor.Error("OpenAIError", `OpenAI HTTP ${res.status}: ${text}`);
  }

  const json = await res.json().catch(() => ({} as any));
  const content = json?.choices?.[0]?.message?.content;
  if (!content) throw new Meteor.Error("OpenAIError", "No content returned by OpenAI.");
  return content as string;
}

// ---- Meteor Method ----
// Frontend calls this with: repoUrl, branchName, contributorName, optional limit (default 20)
Meteor.methods({
  async "ai.summarizeUserCommits"(opts: {
    repoUrl: string;
    branchName: string;
    contributorName: string;
    limit?: number; // default 20
  }) {
    const { repoUrl, branchName, contributorName } = opts || ({} as any);
    const limit = Math.max(1, Math.min(50, opts?.limit ?? 20)); // cap to 50 to avoid giant prompts

    if (!repoUrl || !branchName || !contributorName) {
      throw new Meteor.Error("BadRequest", "repoUrl, branchName and contributorName are required.");
    }

    // 1) Fetch repo data from Haskell API
    const repoData = await fetchRepositoryData(repoUrl);

    // 2) Select last N commits for (branch, user)
    const commits = selectLastCommits(repoData, branchName, contributorName, limit);
    if (commits.length === 0) {
      return {
        repoUrl,
        branchName,
        contributorName,
        commits: [],
        summary: "(No commits found for that contributor on this branch.)"
      };
    }

    // 3) Build prompt and get summary
    const prompt = buildCommitSummaryPrompt(repoUrl, branchName, contributorName, commits);
    const summary = await summariseWithAI(prompt);

    // 4) Return useful, display-ready payload
    return {
      repoUrl,
      branchName,
      contributorName,
      count: commits.length,
      commits: commits.map(c => ({
        hash: c.commitHash,
        title: c.commitTitle,
        timestamp: c.timestamp
      })),
      summary
    };
  }
});
