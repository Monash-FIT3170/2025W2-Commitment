import type { RuleInput } from "./types";


// parse and validate json for backend regex methods
export function parseRulesOrThrow(text: string): RuleInput[] {
  const raw: unknown = JSON.parse(text);
  if (!Array.isArray(raw)) throw new Error("Invalid rules JSON (must be an array)");

  const parsed = raw as RuleInput[];
  const ok = parsed.every(
    (r) =>
      r &&
      typeof r.key === "string" &&
      r.key.trim().length > 0 &&
      typeof r.regex === "string" &&
      typeof r.scale === "number" &&
      (r.sign === "+" || r.sign === "-")
  );

  if (!ok) {
    throw new Error(
      "Invalid rules JSON (each rule needs key:string, regex:string, scale:number, sign:'+'|'-')"
    );
  }
  return parsed;
}

export function ensureSafeFlags(base?: string): string {
  const allowed = ["i", "m", "s", "u"];
  const input = (base ?? "i").split("");
  const out = input.filter(
    (ch, idx, arr) => allowed.includes(ch) && arr.indexOf(ch) === idx
  );
  return out.length ? out.join("") : "i";
}

export function compileRulesForClient(
  rules: RuleInput[]
): { key: string; re: RegExp }[] {
  return rules
    .map((r) => {
      try {
        return { key: r.key, re: new RegExp(r.regex, ensureSafeFlags(r.flags)) };
      } catch {
        return null;
      }
    })
    .filter((x): x is { key: string; re: RegExp } => x !== null);
}
