FULL_COMPILATION = True
VIEW_HASKELL_OUTPUT = False
TESTS = [
	"invalid/semanticErr",
	"invalid/syntaxErr",
	"valid",
]
SKIP_OUTPUT_TESTS_FOR_VALID = True

import subprocess
import argparse
from sys import argv, exit as sys_exit
from os import path as os_path
import pathlib

def extract(text, startText, endText=None):
	start = text.find(startText)
	if start == -1:
		return None
	end = text.find(endText or startText, start + len(startText))
	return text[start + len(startText):end]

def getExpectedExit(waccFilename):
	if f"{waccFilename}"[17:22] == "valid":
		return 0
	if f"{waccFilename}"[17:34] == "invalid/syntaxErr":
		return 100
	return 200


PASSED = "\033[0;32m.\033[0m"
FAILED_EXIT = "\033[0;31mE\033[0m"
FAILED_OUTPUT = "\033[0;31mO\033[0m"
SKIP = "\033[1;33m-\033[0m"

testSummary = ""
failedTests = []
skippedTests = 0

def addTestResult(result, testname='', expected='', actual=''):
	print(result, end='', flush=True)
	if result not in [PASSED, SKIP]:
		failedTests.append((testname, expected, actual))
	return result

totalTests = 0

print(PASSED, "passed")
print(FAILED_EXIT, "wrong exit code")
print(FAILED_OUTPUT, "wrong output")
print(SKIP, "skipped")

for testDir in TESTS:
	print("\n" + testDir)
	for waccFilename in pathlib.Path("./test/integration/" + testDir).rglob("*.wacc"):
		totalTests += 1

		expectedExit = getExpectedExit(waccFilename)
		if FULL_COMPILATION:
			proc = subprocess.run(
				["sh", "compile", waccFilename],
				stdout=None if VIEW_HASKELL_OUTPUT else subprocess.DEVNULL
			)
			actualExit = proc.returncode
			if expectedExit != actualExit:
				testSummary += addTestResult(FAILED_EXIT, waccFilename, "Exit code: " + str(expectedExit), "Exit code: " + str(actualExit))
				continue

		if expectedExit != 0:
			testSummary += addTestResult(PASSED)
			continue

		if SKIP_OUTPUT_TESTS_FOR_VALID and testDir == "valid":
			testSummary += addTestResult(SKIP)
			skippedTests += 1
			continue
		waccCode = open(f"{waccFilename}", 'r').read()
		waccFileOutputRaw = extract(waccCode, "# Output:", "\n\n")

		expectedInput = extract(waccCode, "Input: ", "\n")
		expectedOutput = '\n'.join(line[2:] for line in waccFileOutputRaw.split("\n")) if waccFileOutputRaw else ""
		if expectedOutput:
			if expectedOutput[0] == '\n':
				expectedOutput = expectedOutput[1:]
			if expectedOutput[-1] == '\n':
				expectedOutput = expectedOutput[:-1]
		actualOutput = "awobobobob"

		testSummary += addTestResult(PASSED if actualOutput == expectedOutput else FAILED_OUTPUT, waccFilename, expectedOutput, actualOutput)

print()
for testname, expectedOutput, actualOutput in failedTests:
	print("\033[1;31m--> Failed " + f"{testname}"[17:-5])
	print("\t\u001b[34mExpected:\033[1;33m")
	for line in expectedOutput.split("\n"):
		print("\t\t" + line)
	print("\t\u001b[34mActual:\033[1;33m")
	for line in actualOutput.split("\n"):
		print("\t\t" + line)

passedTests = totalTests - len(failedTests)
if passedTests != totalTests:
	print("\n" + PASSED, "passed")
	print(FAILED_EXIT, "wrong exit code")
	print(FAILED_OUTPUT, "wrong output")
	print(SKIP, "skipped")
	print(testSummary)
print("\n\033[1m\033[0;32m", passedTests, "passed,\033[0;31m", len(failedTests), "failed,\033[1;33m", skippedTests, "skipped." )

print("\033[0m(with" + ("" if FULL_COMPILATION else "out") + " full compilation)\n")
sys_exit(0 if len(failedTests) == 0 else 1)

# result = subprocess.run(
# 	["./test/integration/refCompile", "-a", "-x", waccFilename],
# 	input=waccFileInput + '\n',
# 	capture_output=True,
# 	text=True
# )

# LINE_DIVIDER = "===========================================================\n"
# ASSEMBLY_DIVIDER = ".s contents are:\n" + LINE_DIVIDER
# OUTPUT_DIVIDER = "-- Executing...\n" + LINE_DIVIDER
# EXIT_CODE_TEXT = "\nThe exit code is "

# rawAssembly = extract(result.stdout, ASSEMBLY_DIVIDER, "\n" + LINE_DIVIDER)

# assembly = '\n'.join(line.split('\t', 1)[1] for line in rawAssembly.split("\n")) if rawAssembly else None
# output = extract(result.stdout, OUTPUT_DIVIDER, "\n" + LINE_DIVIDER)
# exitCode = extract(result.stdout, EXIT_CODE_TEXT, ".\n")

# print(assembly)
# if assembly:
# 	with open(os_path.splitext(os_path.basename(waccFilename))[0] + '.s', 'w') as f:
# 		f.write(assembly)
# 		f.close()
