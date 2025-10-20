import React from 'react'
import {useLocalStorage} from "@hook/useLocalStorage";
import ScriptEditor from "@ui/components/scaling/CustomScriptExport/ScriptExecution/ScriptEditor";

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

export default function ScriptSpecification() {
  const [code, setCode] = useLocalStorage('custom-execution-script', initial_script);

  console.log(code);
  return (
    <div>
      <ScriptEditor code={code} setCode={setCode} />

    </div>
  )
}