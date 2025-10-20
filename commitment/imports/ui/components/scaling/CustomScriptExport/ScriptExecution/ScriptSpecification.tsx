import React, { useState } from 'react'
import ScriptEditor from "@ui/components/scaling/CustomScriptExport/ScriptExecution/ScriptEditor";
import {cn} from "@ui/lib/utils";
import {FileCode2} from "lucide-react";

export const initialScript = `import csv

# If you print a List or Tuple, it will be added to the data field returned by the API.
# print(("Joe", 0.8))
# print(["John", 1.12])

# Getting data from the csv:
with open("./data.csv", newline="", encoding="utf-8") as data_csv:
  csv_rows = csv.reader(data_csv)
  headers = next(csv_rows)
  print("found headers: ", headers)
  
  data = [row for row in csv_rows]
  
  idx_name = headers.index("contributor_name")
  idx_total_commits = headers.index("total_commits")
  
  max_total_commits = max((row[idx_total_commits] for row in data))
  
  for row in data:
    # output scaling for each name as total_commits / max_total_commits
    print([row[idx_name], row[idx_total_commits] / max_total_commits])
`;

export interface ScriptSpecificationProps {
  className?: string,
  code?: string,
  setCode?: (code: string) => void,
  icon?: React.ReactNode,
  name?: string
}

/**
 * Component to allow users to specify their desired python source code.
 * DO NOT VARY IF YOU PASS IN props.code AND props.setCode. EITHER PROVIDE IT OR DON'T PROVIDE IT AT ALL
 * @param props
 * @constructor
 */
export default function ScriptSpecification(props: ScriptSpecificationProps) {
  // If code and set code aren't provided, we call a hook ourselves.
  const [code, setCode] = props.code || props.setCode
    ? [props.code ?? "", props.setCode ?? (() => {})]
    : useState<string>(initialScript);
  const [selected, setSelected] = useState<boolean>(false);
  const [dragging, setDragging] = useState<boolean>(false);

  const editorClassName = cn(
    "bg-git-bg-primary rounded-md border-1 outline-1 mt-3 min-h-[50vh] inset-shadow-md overflow-clip",
    "flex flex-col",
    selected
      ? "outline-primary border-primary"
      : "outline-transparent border-git-stroke-secondary",
    dragging
      ? "outline-dashed outline-2 outline-primary animate-pulse"
      : "",
  )

  const icon = props.icon ?? <FileCode2 size='sm'/>;
  const name = props.name ?? "script.py";

  return (
    <div className={editorClassName}>
      <p className="text-git-text-secondary bg-git-bg-bottom text-sm pt-1 pb-0.5 pl-1 pr-3 w-fit border-git-stroke-tertiary border-0 border-b-1 border-e-1  rounded-br-md flex flex-row items-center gap-1 justify-center">
        <div className="text-git-text-secondary opacity-75 pb-1 pt-0.5 w-6 h-6" >
          {icon}
        </div>
        <span>{name}</span>
      </p>
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

        setDragging={setDragging}
      />
    </div>
  )
}