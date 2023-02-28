# view each test's output
VIEW_OUTPUT = False

# specify any test paths in test/integration to test emulation on
# WARNING: if qemu not found, it uses the slow refEmulate
QEMU_TESTS = [
	# "valid/array",
	# "valid/basic/exit",
]

################################################################################

from os import path as os_path, remove, scandir
from pathlib import Path
import subprocess
from itertools import chain

global QEMU_NOT_FOUND
QEMU_NOT_FOUND = False

BLUE = "\033[0;34m"
GREEN = "\033[0;32m"
LIGHT_RED = "\033[1;31m"
RED = "\033[0;31m"
YELLOW = "\033[1;33m"
BOLD = "\033[1m"
END = "\033[0m"

PASSED = GREEN + "." + END
FAILED_COMPILE = RED + "C" + END
FAILED_EXIT = RED + "E" + END
FAILED_OUTPUT = RED + "O" + END
SKIPPED = YELLOW + "-" + END

failedTests = []

QEMU_TEST_PATHS = [Path("test/integration/" + path) for path in QEMU_TESTS]
def shouldTestOutput(testPath):
	return any(runPath in testPath.parents for runPath in QEMU_TEST_PATHS)

def integrationTests():
	print(PASSED, "passed")
	print(FAILED_COMPILE, "failed compilation")
	print(FAILED_EXIT, "wrong exit code")
	print(FAILED_OUTPUT, "wrong output")
	print(SKIPPED, "skipped")
	testSummary = ""
	skippedTests = totalTests = 0
	
	wacc40exe = str(next(Path("./.stack-work/dist").rglob("build/WACC40-exe/WACC40-exe"), ""))
	if not wacc40exe:
		print(RED + "\nWACC40-exe not found - did you", BOLD + "stack build" + END + RED + "?", END)
		exit(1)

	for testGroup in chain(scandir("./test/integration/invalid"), scandir("./test/integration/valid")):
		print(BOLD, BLUE, "\n", str(Path(testGroup))[17:], END)
		for waccFilename in Path(testGroup).rglob("*.wacc"):
			totalTests += 1
			result = subprocess.run(
				[wacc40exe, waccFilename],
				stdout=None if VIEW_OUTPUT else subprocess.DEVNULL
			)

			expectedExit = getExpectedExit(waccFilename)
			actualExit = result.returncode
			if expectedExit != actualExit:
				testSummary += addTestResult(FAILED_COMPILE, waccFilename, f"Exit code: {expectedExit}", f"Exit code: {actualExit} (toggle {BOLD}VIEW_OUTPUT{END} for more info)")
				continue
			if expectedExit != 0:
				testSummary += addTestResult(PASSED)
				continue
			basename = os_path.splitext(os_path.basename(waccFilename))[0]

			if shouldTestOutput(waccFilename):
				expectedInput, expectedOutput = getWaccFileIO(waccFilename)
				actualOutput, actualExit = getActualOutput(basename, expectedInput)
				if actualExit == 0:
					testSummary += addTestResult(PASSED if actualOutput == expectedOutput else FAILED_OUTPUT, waccFilename, expectedOutput, actualOutput)
				else:
					testSummary += addTestResult(FAILED_EXIT, waccFilename, f"Exit code: {expectedExit}", f"Exit code: {actualExit} (toggle {BOLD}VIEW_OUTPUT{END} for more info)")
			else:
				testSummary += addTestResult(SKIPPED)
				skippedTests += 1

			remove(basename + ".s")


	print()
	for testname, expectedOutput, actualOutput in failedTests:
		print(LIGHT_RED, "--> Failed " + f"{testname}"[17:-5])
		print(YELLOW, "\t", "Expected:", END)
		for line in expectedOutput.split("\n"):
			print("\t\t" + line)
		print(YELLOW, "\t", "Actual:", END)
		for line in actualOutput.split("\n"):
			print("\t\t" + line)

	passedTests = totalTests - len(failedTests) - skippedTests
	if len(failedTests) > 0:
		print("\n" + PASSED, "passed")
		print(FAILED_COMPILE, "failed compilation")
		print(FAILED_EXIT, "wrong exit code")
		print(FAILED_OUTPUT, "wrong output")
		print(SKIPPED, "skipped")
		print(testSummary)

	print(BOLD, GREEN, "\n", passedTests, "passed," + RED, len(failedTests), "failed," + YELLOW, skippedTests, "skipped.", END)
	if QEMU_NOT_FOUND:
		print("qemu tests were run by refEmulate as command not found")

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
	expectedInput = extract(waccCode, "# Input: ", "\n")
	expectedOutput = '\n'.join(line[2:] for line in waccFileOutputRaw.split("\n")) if waccFileOutputRaw else ""
	if expectedOutput:
		if expectedOutput[0] == '\n':
			expectedOutput = expectedOutput[1:]
		if expectedOutput[-1] == '\n':
			expectedOutput = expectedOutput[:-1]
	return expectedInput, expectedOutput

def getActualOutput(basename, waccInput):
	try:
		result1 = subprocess.run(
			["arm-linux-gnueabi-gcc", "-o", basename, "-mcpu=arm1176jzf-s", "-mtune=arm1176jzf-s", basename + ".s"],
		)
		result2 = subprocess.run(
			["qemu-arm", "-L", "/usr/arm-linux-gnueabi/", basename],
			input=(waccInput or '') + '\n',
			capture_output=True,
			text=True
		)
		remove(basename)
		return result2.stdout, result2.returncode
	except FileNotFoundError as e:
		if e.filename == "arm-linux-gnueabi-gcc":
			global QEMU_NOT_FOUND
			QEMU_NOT_FOUND = True
			return runRefEmulator(basename + ".s", waccInput)
		else:
			raise e

def addTestResult(result, testname='', expected='', actual=''):
	print(result, end='', flush=True)
	if result not in [PASSED, SKIPPED]:
		failedTests.append((testname, expected, actual))
	return result


def runRefCompiler(waccFilename, waccFileInput):
	result = subprocess.run(
		["ruby", "test/integration/refCompile", "-a", "-x", waccFilename],
		input=waccFileInput + '\n',
		capture_output=True,
		text=True
	)

	LINE_DIVIDER = "===========================================================\n"
	ASSEMBLY_DIVIDER = ".s contents are:\n" + LINE_DIVIDER
	OUTPUT_DIVIDER = "-- Executing...\n" + LINE_DIVIDER
	EXIT_CODE_TEXT = "\nThe exit code is "

	rawAssembly = extract(result.stdout, ASSEMBLY_DIVIDER, "\n" + LINE_DIVIDER)

	assembly = '\n'.join(line.split('\t', 1)[1] for line in rawAssembly.split("\n")) if rawAssembly else None
	output = extract(result.stdout, OUTPUT_DIVIDER, "\n" + LINE_DIVIDER)
	exitCode = extract(result.stdout, EXIT_CODE_TEXT, ".\n")
	if assembly:
		with open(os_path.splitext(os_path.basename(waccFilename))[0] + '.s', 'w') as f:
			f.write(assembly)
			f.close()

def runRefEmulator(assemblyFile, assemblyInput):
	result = subprocess.run(
		["ruby", "test/integration/refEmulate", assemblyFile],
		input=(assemblyInput or '') + '\n',
		capture_output=True,
		text=True
	)
	LINE_DIVIDER = "---------------------------------------------------------------\n"
	ASSEMBLY_OUTPUT_DIVIDER = "-- Assembly Output:\n"
	OUTPUT_DIVIDER = "-- Emulation Output:\n"
	EXIT_CODE_TEXT = LINE_DIVIDER + "The exit code is: "
	assemblyOutput = extract(result.stdout, ASSEMBLY_OUTPUT_DIVIDER, "\n\n" + OUTPUT_DIVIDER)
	if assemblyOutput != "" and VIEW_OUTPUT:
		print(assemblyOutput)
	output = extract(result.stdout, OUTPUT_DIVIDER, "\n" + LINE_DIVIDER)
	exitCode = extract(result.stdout, EXIT_CODE_TEXT, ".\n")
	return output, int(exitCode)

integrationTests()

