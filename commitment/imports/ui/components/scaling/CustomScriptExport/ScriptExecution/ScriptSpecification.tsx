import React, { useState } from 'react'
import ScriptEditor from "@ui/components/scaling/CustomScriptExport/ScriptExecution/ScriptEditor";
import {cn} from "@ui/lib/utils";

export const initialScript = `import csv

# If you print a List or Tuple, it will be added to the data field returned by the API.
# print(("Joe", 0.8))
# print(["John", 1.12])

# Getting data from the csv:
with open("./data.csv", newline="", encoding="utf-8") as data_csv:
  csv_rows = csv.reader(data_csv)
  headers = next(csv_rows)
  data = [row for row in csv_rows]
  
  idx_name = headers.index("contributor_name")
  idx_total_commits = headers.index("total_commits")
  
  max_total_commits = max((int(row[idx_total_commits]) for row in data))
  
  for row in data:
    # output scaling for each name as total_commits / max_total_commits
    print([row[idx_name], int(row[idx_total_commits]) / max_total_commits])
    
`;

export interface ScriptSpecificationProps {
  className?: string | string[],
  code: string | string[],
  setCode: ((code: string) => void) | ((code: string) => void)[],
  icon?: React.ReactNode | React.ReactNode[],
  name?: string | string[],
  readonly?: boolean | boolean[],
  language?: string | string[],
  children?: React.ReactNode,
  padding?: number | number[],
}

function asArrayDefault<T>(value: T | T[], defaultValue: T, tabCount: number | undefined = undefined): T[] {
  if (tabCount === undefined)
    tabCount = Array.isArray(value) ? value.length : 1;
  if (value === undefined)
    return tabCount === 1 ? [value] : Array(tabCount).fill(defaultValue);

  if (Array.isArray(value)) {
    return value.length < tabCount
      ? [...value, ...Array(tabCount-1).fill(defaultValue)]
      : value;
  }
  return tabCount === 1 ? [value] : Array(tabCount-1).fill(value);
}
function asArray<T>(value?: T | T[], tabCount?: number): (T | undefined)[] {
  return asArrayDefault<T | undefined>(value, undefined, tabCount);
}

/**
 * Component to allow users to specify their desired python source code.
 * DO NOT VARY IF YOU PASS IN props.code AND props.setCode. EITHER PROVIDE IT OR DON'T PROVIDE IT AT ALL
 * @param props
 * @constructor
 */
export default function ScriptSpecification(props: ScriptSpecificationProps) {
  const codes = asArrayDefault(props.code, "").map(
    code => code ?? initialScript
  );
  const tabCount = codes.length;

  // Unpack props used across all tabs
  const icons = asArray(props.icon, tabCount);
  const names = asArray(props.name, tabCount);

  // The currently open editor tab
  const [tabIndexRaw, setTabIndex] = useState<number>(0);
  const tabIndex = tabIndexRaw < 0
    ? 0
    : tabIndexRaw >= tabCount
      ? tabCount - 1
      : tabIndexRaw;

  // Current tab props
  const code = codes[tabIndex];
  const setCode = asArray(props.setCode, tabCount)[tabIndex] ?? (() => {});
  const readonly = asArray(props.readonly, tabCount)[tabIndex];
  const language = asArray(props.language, tabCount)[tabIndex] ?? "python";
  const className = asArray(props.className, tabCount)[tabIndex];
  const padding = asArray(props.padding, tabCount)[tabIndex];

  // UI State
  const [selectedRaw, setSelected] = useState<boolean>(false);
  const [draggingRaw, setDragging] = useState<boolean>(false);

  const selected = selectedRaw && !readonly;
  const dragging = draggingRaw && !readonly;

  const editorClassName = cn(
    "bg-git-bg-primary rounded-md border-1 outline-1 mt-3 inset-shadow-sm overflow-clip",
    "flex flex-col",
    selected
      ? "outline-primary border-primary"
      : "outline-transparent border-git-stroke-secondary",
    dragging
      ? "outline-dashed outline-2 outline-primary animate-pulse"
      : "",
    className
  )

  const tabs = names.map((name, i) => (
    <div
      className={cn(
        "text-git-text-secondary bg-git-bg-bottom border-git-stroke-tertiary border-b-1 border-e-1 rounded-br-md",
        "transition-transform ease-out duration-100",
        tabCount > 1 && tabIndex === i
          ? "z-10 border-w-1 shadow-sm translate-y-0"
          : "text-git-text-secondary bg-git-bg-bottom/75 -translate-y-0.5",
        i > 0 && "rounded-bl-md",

        tabCount > 1 && tabIndex !== i && "cursor-pointer hover:bg-git-bg hover:text-git-text-primary",
        tabCount > 1 && tabIndex === i && "cursor-default"
      )}
      onClick={() => setTabIndex(i)}
    >
      <div
        className={cn(
          "text-sm pb-0.5 pt-1 pl-1 pr-3 w-fit flex flex-row items-center gap-1 justify-center",
          tabCount > 1 && tabIndex !== i && "opacity-60",
        )}
      >
        <div className="opacity-75 pb-1 pt-0.5 w-6 h-6" >
          {icons[i]}
        </div>
        <span>{name ?? "script.py"}</span>
      </div>
    </div>
  ));

  return (
    <div className={editorClassName}>
      <div className="flex flex-row items-center justify-start p-0">
        {tabs}
      </div>
      <ScriptEditor
        className="grow h-auto text-sm"
        code={code}
        setCode={setCode}
        onBlur={() => {
          setSelected(false)
          setDragging(false)
        }}
        onFocus={() => {
          setSelected(true)
          setDragging(false)
        }}

        setDragging={readonly ? undefined : setDragging}
        readonly={readonly}
        language={language}
        padding={padding ?? 10}
      />
      {props.children}
    </div>
  )
}