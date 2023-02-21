VIEW_HASKELL_OUTPUT = False
TESTS = [
	"invalid/semanticErr",
	"invalid/syntaxErr",
	"valid",
]

from os import path as os_path, remove
from pathlib import Path
import subprocess
from sys import argv

SKIP_OUTPUT_TESTS = True # len(argv) < 2 or argv[1] != "-o"

BLUE = "\033[0;34m"
GREEN = "\033[0;32m"
LIGHT_RED = "\033[1;31m"
RED = "\033[0;31m"
YELLOW = "\033[1;33m"
BOLD = "\033[1m"
END = "\033[0m"

PASSED = GREEN + "." + END
FAILED_EXIT = RED + "E" + END
FAILED_OUTPUT = RED + "O" + END
SKIPPED = YELLOW + "-" + END

failedTests = []

def integrationTests():
	print(PASSED, "passed")
	print(FAILED_EXIT, "wrong exit code")
	print(FAILED_OUTPUT, "wrong output")
	print(SKIPPED, "skipped")
	testSummary = ""
	skippedTests = totalTests = 0
	
	wacc40exe = next(Path("./.stack-work/dist").rglob("build/WACC40-exe/WACC40-exe"), None)
	if not wacc40exe:
		print(RED + "\nWACC40-exe not found - did you", BOLD + "stack build" + END + RED + "?", END)
		exit(1)

	for testDir in TESTS:
		print("\n" + testDir)
		for waccFilename in Path("./test/integration/" + testDir).rglob("*.wacc"):
			totalTests += 1
			result = subprocess.run(
				[str(wacc40exe), waccFilename],
				stdout=None if VIEW_HASKELL_OUTPUT else subprocess.DEVNULL
			)

			expectedExit = getExpectedExit(waccFilename)
			actualExit = result.returncode
			if expectedExit != actualExit:
				testSummary += addTestResult(FAILED_EXIT, waccFilename, f"Exit code: {expectedExit}", f"Exit code: {actualExit}")
				continue
			testSummary += addTestResult(PASSED)
			if expectedExit != 0:
				continue

			basename = os_path.splitext(os_path.basename(waccFilename))[0]

			if SKIP_OUTPUT_TESTS:
				testSummary += addTestResult(SKIPPED)
				skippedTests += 1
				remove(basename + ".s")
				continue

			expectedInput, expectedOutput = getWaccFileIO(waccFilename)
			actualOutput = getActualOutput(basename, expectedInput)

			testSummary += addTestResult(PASSED if actualOutput == expectedOutput else FAILED_OUTPUT, waccFilename, expectedOutput, actualOutput)

	print()
	for testname, expectedOutput, actualOutput in failedTests:
		print(LIGHT_RED, "--> Failed " + f"{testname}"[17:-5])
		print(YELLOW, "\t", "Expected:", END)
		for line in expectedOutput.split("\n"):
			print("\t\t" + line)
		print(YELLOW, "\t", "Actual:", END)
		for line in actualOutput.split("\n"):
			print("\t\t" + line)

	passedTests = totalTests - len(failedTests)
	if passedTests != totalTests:
		print("\n" + PASSED, "passed")
		print(FAILED_EXIT, "wrong exit code")
		print(FAILED_OUTPUT, "wrong output")
		print(SKIPPED, "skipped")
		print(testSummary)

	print(BOLD, GREEN, "\n", passedTests, "passed," + RED, len(failedTests), "failed," + YELLOW, skippedTests, "skipped.", END)

	exit(0 if len(failedTests) == 0 else 1)


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

def getWaccFileIO(waccFilename):
	waccCode = open(f"{waccFilename}", 'r').read()
	waccFileOutputRaw = extract(waccCode, "# Output:", "\n\n")
	expectedInput = extract(waccCode, "Input: ", "\n")
	expectedOutput = '\n'.join(line[2:] for line in waccFileOutputRaw.split("\n")) if waccFileOutputRaw else ""
	if expectedOutput:
		if expectedOutput[0] == '\n':
			expectedOutput = expectedOutput[1:]
		if expectedOutput[-1] == '\n':
			expectedOutput = expectedOutput[:-1]
	return expectedInput, expectedOutput

def getActualOutput(basename, waccInput):
	result1 = subprocess.run(
		["arm-linux-gnueabi-gcc", "-o", basename, "-mcpu=arm1176jzf-s", "-mtune=arm1176jzf-s", basename + ".s"],
	)
	remove(basename + ".s")
	result2 = subprocess.run(
		["qemu-arm", "-L", "/usr/arm-linux-gnueabi/", basename],
		input=(waccInput or '') + '\n',
		capture_output=True,
		text=True
	)
	return result2.stdout

def addTestResult(result, testname='', expected='', actual=''):
	print(result, end='', flush=True)
	if result not in [PASSED, SKIPPED]:
		failedTests.append((testname, expected, actual))
	return result

integrationTests()

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
