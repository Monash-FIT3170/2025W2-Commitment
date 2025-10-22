// commitment/server/ai_commit_summary.ts

import { Meteor } from 'meteor/meteor';
import { DDPRateLimiter } from "meteor/ddp-rate-limiter";
// import "meteor/fetch"; // ensure server-side fetch

console.log("[ai_commit_summary] registering Meteor methods");

// ---------- Types aligned with your Haskell JSON ----------
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
  timestamp: string; // ISO-8601
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
  branches: BranchData[];
  allCommits: Record<string, CommitData>;
  contributors: Record<string, ContributorData>;
}>;

// ---------- Config ----------
const API_BASE = process.env.HASKELL_API_BASE ?? "http://localhost:8081";
const GEMINI_API_KEY = process.env.GEMINI_API_KEY ?? "";
const GEMINI_MODEL = process.env.GEMINI_MODEL ?? "gemini-2.5-flash";
const GEMINI_API_VER = (process.env.GEMINI_API_VERSION ?? "v1").trim();

console.log(`[ai] provider=gemini model=${GEMINI_MODEL} ver=${GEMINI_API_VER} key?=${GEMINI_API_KEY ? "yes" : "no"}`);

// Prefer global fetch; fall back to imported fetch if needed
const _fetch: typeof fetch = (globalThis as any).fetch || (fetch as any);

// ---------- Helpers ----------
const isNonEmptyString = (s: unknown): s is string =>
  typeof s === "string" && s.trim().length > 0;

function parseIso(s: string): number {
  const t = Date.parse(s);
  return Number.isNaN(t) ? 0 : t;
}

function getBranchByName(data: RepositoryData, branchName: string): BranchData | null {
  return data.branches.find(b => b.branchName === branchName) ?? null;
}

function truncate(s: string, max = 280): string {
  if (!s) return "";
  return s.length <= max ? s : s.slice(0, max - 3) + "...";
}

function hardLimitText(s: string, maxChars: number): string {
  if (s.length <= maxChars) return s;
  return s.slice(0, maxChars) + "\n[truncated]";
}

// ---------- Fetch repository data from Haskell HTTP API ----------
async function fetchRepositoryData(repoUrl: string): Promise<RepositoryData> {
  const res = await _fetch(API_BASE, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ url: repoUrl })
  });

  if (!res.ok) {
    const text = await res.text().catch(() => "");
    throw new Meteor.Error("APIError", `Haskell API ${res.status}: ${text || "no body"}`);
  }

  const json: any = await res.json().catch(() => ({}));
  if (!json || json.type !== "value" || !json.data) {
    throw new Meteor.Error("APIError", "Unexpected Haskell API payload (expected {type:'value', data:...}).");
  }
  return json.data as RepositoryData;
}

// ---------- Select last N commits for (branch, user) ----------
function selectLastCommits(
  data: RepositoryData,
  branchName: string,
  contributorName: string,
  limit: number
): CommitData[] {
  const branch = getBranchByName(data, branchName);
  if (!branch) return [];

  const commitsOnBranch: CommitData[] = branch.commitHashes
    .map(h => data.allCommits[h])
    .filter((c): c is CommitData => Boolean(c));

  const byUser = commitsOnBranch
    .filter(c => c.contributorName === contributorName)
    .sort((a, b) => parseIso(b.timestamp) - parseIso(a.timestamp));

  return byUser.slice(0, Math.max(1, Math.min(50, limit))); // cap to 50
}

function heuristicFallback(prompt: string, prefix = "Heuristic summary"): string {
  const bullets = prompt
    .split("\n")
    .filter(l => l.startsWith("#"))
    .map(l => l.replace(/^#\d+\s+/, "").trim())
    .slice(0, 6)
    .map(s => `• ${s}`);
  return `${prefix}:\n${bullets.join("\n")}`;
}

// ---------- Build prompt (with caps) ----------
function buildPrompt(repoUrl: string, branch: string, user: string, commits: CommitData[]): string {
  const header =
    `Summarise these recent commits for contributor "${user}" on branch "${branch}" of repo ${repoUrl}.
Provide 4–6 concise bullet points highlighting themes, feature areas, bugfixes, refactors, risks, or TODOs.
Keep it neutral and factual.
`;

  const lines = commits.map((c, i) => {
    const title = truncate(c.commitTitle || "(no title)", 160);
    const desc  = truncate(c.description || "", 400);
    return `#${i + 1} [${c.timestamp}] ${c.commitHash.slice(0,7)} — ${title}${desc ? `\n    ${desc}` : ""}`;
  });

  const prompt = `${header}\n${lines.join("\n")}`;
  return hardLimitText(prompt, 18_000);
}

// ---------- OpenAI call with retries ----------
// async function summariseWithOpenAI(prompt: string): Promise<string> {
//   if (!OPENAI_API_KEY) {
//     // Deterministic fallback so your UI still works without a key
//     const bullets = prompt
//       .split("\n")
//       .filter(l => l.startsWith("#"))
//       .map(l => l.replace(/^#\d+\s+/, "").trim())
//       .slice(0, 6)
//       .map(s => `• ${s}`);
//     return `No OPENAI_API_KEY set. Heuristic summary:\n${bullets.join("\n")}`;
//   }

//   const headers = {
//     Authorization: `Bearer ${OPENAI_API_KEY}`,
//     "Content-Type": "application/json"
//   };
//   const body = JSON.stringify({
//     model: OPENAI_MODEL,
//     messages: [
//       { role: "system", content: "You are a concise software engineering commit summarizer." },
//       { role: "user", content: prompt }
//     ],
//     temperature: 0.2,
//     max_tokens: 400
//   });

//   let lastErr: any = null;
//   for (let attempt = 0; attempt < 3; attempt++) {
//     try {
//       const res = await _fetch("https://api.openai.com/v1/chat/completions", {
//         method: "POST",
//         headers,
//         body
//       });
//       if (!res.ok) {
//         const text = await res.text().catch(() => "");
//         if (res.status >= 500) { // transient
//           lastErr = new Error(`OpenAI ${res.status}: ${text}`);
//           await new Promise(r => setTimeout(r, 250 * (attempt + 1)));
//           continue;
//         }
//         throw new Meteor.Error("OpenAIError", `OpenAI ${res.status}: ${text || "no body"}`);
//       }
//       const json: any = await res.json().catch(() => ({}));
//       const content = json?.choices?.[0]?.message?.content;
//       if (!content) throw new Meteor.Error("OpenAIError", "No content returned by OpenAI.");
//       return content as string;
//     } catch (e) {
//       lastErr = e;
//       await new Promise(r => setTimeout(r, 250 * (attempt + 1)));
//     }
//   }
//   throw new Meteor.Error("OpenAIError", String(lastErr || "OpenAI request failed"));
// }

async function summariseWithGemini(prompt: string): Promise<string> {
  const apiKey  = (process.env.GEMINI_API_KEY || GEMINI_API_KEY || "").trim();
  const model   = (process.env.GEMINI_MODEL || GEMINI_MODEL || "gemini-2.5-flash").trim();
  const version = (process.env.GEMINI_API_VERSION || "v1").trim(); // default v1
  const shouldDebug = (process.env.NODE_ENV !== "production") || process.env.GEMINI_DEBUG === "1";

  if (!apiKey) {
    return heuristicFallback(prompt, "No GEMINI_API_KEY set");
  }

  const callGemini = async (apiVersion: string) => {
    const url = `https://generativelanguage.googleapis.com/${apiVersion}/models/${encodeURIComponent(model)}:generateContent?key=${encodeURIComponent(apiKey)}`;
    return _fetch(url, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({
        contents: [{ parts: [{ text: prompt }] }],
        generationConfig: { temperature: 0.2, maxOutputTokens: 400 }
      })
    });
  };

  // First try configured version
  let res = await callGemini(version);

  // If someone set v1beta but the model lives on v1, retry on v1 transparently
  if (res.status === 404 && version.toLowerCase() !== "v1") {
    res = await callGemini("v1");
  }

  if (!res.ok) {
    const txt = await res.text().catch(() => "");
    // Quota/permission → graceful fallback
    if (res.status === 429 || res.status === 403) {
      return heuristicFallback(prompt, `Gemini quota/permission issue (${res.status})`);
    }
    // Transient 5xx → one quick retry
    if (res.status >= 500 && res.status <= 599) {
      await new Promise(r => setTimeout(r, 300));
      const again = await callGemini(version);
      if (!again.ok) {
        const t2 = await again.text().catch(() => "");
        return heuristicFallback(prompt, `Gemini temporarily unavailable (${again.status}): ${t2 || "no body"}`);
      }
      const j2: any = await again.json().catch(() => ({}));
      if (shouldDebug) console.log("[gemini debug retry]", JSON.stringify(j2).slice(0, 1200));
      const tParsed =
        j2?.candidates?.[0]?.content?.parts
          ?.map((p: any) => p?.text ?? "")
          .join("")
          .trim();
      return tParsed || heuristicFallback(prompt, "Gemini returned no content");
    }
    // Other client error → fallback (don’t break UI)
    return heuristicFallback(prompt, `Gemini ${res.status}: ${txt || "no body"}`);
  }

  const json: any = await res.json().catch(() => ({}));
  if (shouldDebug) console.log("[gemini debug]", JSON.stringify(json).slice(0, 1200));

  const text =
    json?.candidates?.[0]?.content?.parts
      ?.map((p: any) => p?.text ?? "")
      .join("")
      .trim();

  if (!text) {
    console.warn("[gemini warn] No text content in response");
    return heuristicFallback(prompt, "Gemini returned no content");
  }

  return text;
}


// ---------- Meteor method ----------
Meteor.methods({
  // Client: Meteor.call("ai.summarizeUserCommits", { repoUrl, branchName, contributorName, limit? }, cb)
  async "ai.summarizeUserCommits"(opts: {
    repoUrl: string;
    branchName: string;
    contributorName: string;
    limit?: number; // default 20 (capped at 50)
  }) {
    const repoUrl = opts?.repoUrl?.trim();
    const branchName = opts?.branchName?.trim();
    const contributorName = opts?.contributorName?.trim();
    const limit = Math.max(1, Math.min(50, Number(opts?.limit ?? 20)));

    if (!isNonEmptyString(repoUrl) || !isNonEmptyString(branchName) || !isNonEmptyString(contributorName)) {
      throw new Meteor.Error("BadRequest", "repoUrl, branchName, and contributorName are required.");
    }

    const data = await fetchRepositoryData(repoUrl);
    const commits = selectLastCommits(data, branchName, contributorName, limit);

    if (commits.length === 0) {
      return {
        repoUrl, branchName, contributorName, count: 0,
        commits: [],
        summary: "(No commits found for that contributor on this branch.)"
      };
    }

    const prompt = buildPrompt(repoUrl, branchName, contributorName, commits);
    const summary = await summariseWithGemini(prompt);

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

// ---------- Rate limit ----------
DDPRateLimiter.addRule(
  { name: "ai.summarizeUserCommits", type: "method" },
  10,     // max calls
  10_000  // per 10 seconds
);