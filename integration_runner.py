import pathlib
import re
import subprocess
import sys

# tests = [("invalid/syntaxErr/", 100),
tests = [("invalid/semanticErr/expressions/", 1),
         ("valid/expressions/", 0)]

base = "test/integration/"

passing = 0
total = 0

failed_list = []

for (test_entry, expected) in tests:
  for fname in pathlib.Path(base + test_entry).rglob("*.wacc"):
    proc = subprocess.run(["sh", "compile", fname], stdout=subprocess.DEVNULL)

    # Return code check
    actual = proc.returncode
    total += 1
    if actual == expected:
      passing += 1
    else:
      failed_list.append(fname)
      print(f"Failed test {fname}. Expected exit code {expected} but got {actual}")


print(f"\n\n{'*' * 10}")
for f in failed_list:
  print(f)
print(f"\n\n{'*' * 10}")
print(f"Finished running tests. {passing} / {total} tests passed.")
sys.exit(passing != total)