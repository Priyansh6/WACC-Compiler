import glob
import re
import subprocess

tests = ["valid/basic/skip/comment.wacc",
         "invalid/syntaxErr/basic/badComment.wacc",
         "invalid/syntaxErr/if/*"]

base = "test/integration/"

def get_return_code(fname):
  with open(fname) as f:
    lines = f.readlines()
    for i in range(len(lines)):
      if lines[i].startswith("# Exit:"):
        return int(re.search("[0-9]+", lines[i+1]).group())
  return 0

passing = 0
total = 0

for test_entry in tests:
  for fname in glob.glob(base + test_entry):
    proc = subprocess.run(["../../compile", fname], stdout=subprocess.DEVNULL)

    # Return code check
    actual = proc.returncode
    expected = get_return_code(fname)
    total += 1
    if actual == expected:
      passing += 1
    else:
      print(f"Failed test {fname}. Expected exit code {expected} but got {actual}")

print(f"Finished running tests. {passing} / {total} tests passed.")