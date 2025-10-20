import React from 'react'
import {useLocalStorage} from "@hook/useLocalStorage";
import ScriptEditor from "@ui/components/scaling/CustomScriptExport/ScriptExecution/ScriptEditor";
import {cn} from "@ui/lib/utils";

const initial_script = `
import os
import csv

print("This is an example script output")

print("If you print a List or Tuple, it will be added to the data field returned by the API.")
print(("Joe", 0.8))
print(["John", 1.12])

print("Displaying all files in current directory:")
current_dir = os.getcwd()
files = [f for f in os.listdir(current_dir) if os.path.isfile(os.path.join(current_dir, f))]

# Print each file
for f in files:
  print(f)

print("Getting data from the csv:")
with open("./data.csv", newline="", encoding="utf-8") as data_csv:
  data = csv.reader(data_csv)
  for row in data:
    # each row is a list of strings
    print(row)
`;

export interface ScriptSpecificationProps {
  className?: string,
  code?: string,
  setCode?: (code: string) => void,
}

/**
 * Component to allow users to specify their desired python source code.
 * DO NOT VARY IF YOU PASS IN props.code AND props.setCode. EITHER PROVIDE IT OR DON'T PROVIDE IT AT ALL
 * @param props
 * @constructor
 */
export default function ScriptSpecification(props: ScriptSpecificationProps) {
  // If code and set code aren't provided, we call a hook ourselves.
  const [code, setCode] = props.code && props.setCode
    ? [props.code, props.setCode]
    : useLocalStorage('custom-execution-script', initial_script);
  const [selected, setSelected] = React.useState<boolean>(false);
  const [dragging, setDragging] = React.useState<boolean>(false);

  const editorClassName = cn(
    "bg-git-bg-bottom rounded-md border-1 outline-1 mt-3 min-h-[50vh] inset-shadow-md overflow-clip",
    "flex flex-col",
    selected
      ? "outline-primary border-primary"
      : "outline-transparent border-git-stroke-secondary",
    dragging
      ? "outline-dashed outline-2 outline-primary animate-pulse"
      : "",
  )

  return (
    <div className={editorClassName}>
      <p className="text-git-text-secondary bg-git-bg-primary text-sm pt-1 pb-0.5 px-3 w-fit border-git-stroke-tertiary border-0 border-b-1 border-e-1  rounded-br-md flex flex-row items-center gap-3">
        <span>script.py</span>
      </p>
      <ScriptEditor
        className="grow h-auto text-sm"
        code={code}
        setCode={setCode}
        onBlur={() => setSelected(false)}
        onFocus={() => setSelected(true)}
        setDragging={setDragging}
      />
    </div>
  )
}