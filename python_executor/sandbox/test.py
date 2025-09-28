#!/usr/bin/python3
import subprocess

i = 0
while True:
  result = subprocess.run(
    ["python3", "-c", f"print({i + 1} ** 2.5)"],          # Command as a list
    capture_output=True,   # Capture stdout and stderr
    text=True              # Return output as a string instead of bytes
  )
  exec(f"print(\"Got output: {result.stdout.strip()} {result.stderr.strip()}\")")
  i+=1
print("Done!")
