import pathlib
import re
import subprocess
import sys

tests = [
    ("invalid/syntaxErr", 100),
    ("invalid/semanticErr", 200),
    ("valid", 0),
]

base = "test/integration/"

passing = 0
total = 0

failed_list = []

for test_entry, expected in tests:
    for fname in pathlib.Path(base + test_entry).rglob("*.wacc"):
        testname = f"{fname}"[17:-5]
        print("\u001b[34m" + testname + "\u001b[0m")
        proc = subprocess.run(
            ["sh", "compile", fname],
            # stdout=subprocess.DEVNULL # comment line to view haskell output
        )

        # Return code check
        actual = proc.returncode
        total += 1
        if actual == expected:
            passing += 1
        else:
            failed_list.append(fname)
            print(
                f" \u001b[31m\tExpected exit:\t\u001b[31;1m{expected}\u001b[0m\u001b[31m\n\tActual exit:\t\u001b[31;1m{actual}\u001b[0m\n"
            )
    # print("\n")


# print(f"\n\n{'*' * 10}")
# for f in failed_list:
#     print(f)
print(f"\n\n{'*' * 10}")
print(f"{passing} / {total} tests passed.")
sys.exit(passing != total)
