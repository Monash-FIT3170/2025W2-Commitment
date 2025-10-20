import React from "react";

type Props = {
  rulesText: string;
  onChange: (v: string) => void;
};

// text area for json rules
export default function RulesEditor({ rulesText, onChange }: Props): React.JSX.Element {
  return (
    <div className="w-full">
      <span className="text-sm text-git-text-secondary mb-1 block">Rules JSON</span>
      <textarea
        value={rulesText}
        onChange={(e) => onChange(e.target.value)}
        rows={10}
        className="w-full p-3 rounded-lg border-2 border-git-stroke-primary/40 bg-git-bg-elevated text-foreground"
        spellCheck={false}
      />
    </div>
  );
}