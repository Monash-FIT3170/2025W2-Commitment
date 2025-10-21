export type RuleInput = {
  key: string;
  regex: string;
  scale: number;
  sign: "+" | "-";
  flags?: string;
};

export type ResultRow = {
  contributor: string;
  commits: number;
  matchesByRule: Record<string, number>;
  scoreRaw: number;
  scorePerCommit: number;
  suggestedScale: number;
};

export type DetailCommit = {
  title: string;
  matchedKeys: string[];
};
