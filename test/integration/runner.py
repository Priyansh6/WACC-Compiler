# view each test's stdout and/or stderr
VIEW_STDOUT = 0
VIEW_STDERR = 0
TIMEOUT_DURATION = 5 # seconds

# specify any test paths to run any tests on
TESTS = [
	"invalid",
	"valid"
]
# specify any test paths in test/integration to test emulation on
QEMU_TESTS = [
	# "valid/advanced",
	"valid/array/array.wacc",
	"valid/array/arrayBasic.wacc",
	"valid/array/arrayEmpty.wacc",
	# "valid/array/arrayIndexMayBeArrayIndex.wacc",
	"valid/array/arrayLength.wacc",
	"valid/array/arrayLookup.wacc",
	"valid/array/arrayNested.wacc",
	"valid/array/arrayOnHeap.wacc",
	"valid/array/arrayPrint.wacc",
	"valid/array/arraySimple.wacc",
	"valid/array/emptyArrayAloneIsFine.wacc",
	"valid/array/emptyArrayNextLine.wacc",
	"valid/array/emptyArrayPrint.wacc",
	"valid/array/emptyArrayReplace.wacc",
	"valid/array/emptyArrayScope.wacc",
	"valid/array/free.wacc",
	"valid/array/lenArrayIndex.wacc",
	"valid/array/modifyString.wacc",
	"valid/array/printRef.wacc",
	"valid/basic",
	"valid/expressions",
	"valid/function",
	"valid/if",
	"valid/IO",
	"valid/pairs/checkRefPair.wacc",
	"valid/pairs/createPair03.wacc",
	"valid/pairs/createRefPair.wacc",
	"valid/pairs/linkedList.wacc",
	"valid/pairs/null.wacc",
	"valid/pairs/printNullPair.wacc",
	"valid/pairs/printPairOfNulls.wacc",
	"valid/pairs/readPair.wacc",
	"valid/pairs/writeSnd.wacc",
	"valid/pairs/createPair02.wacc",
	"valid/pairs/createPair.wacc",
	"valid/pairs/free.wacc",
	"valid/pairs/nestedPair.wacc",
	# "valid/pairs/nestedPairLeftAssign.wacc",
	# "valid/pairs/nestedPairRightExtract.wacc",
	"valid/pairs/pairarray.wacc",
	"valid/pairs/printNull.wacc",
	"valid/pairs/printPair.wacc",
	"valid/pairs/writeFst.wacc",
	"valid/runtimeErr/arrayOutOfBounds",
	"valid/runtimeErr/divideByZero",
	"valid/runtimeErr/integerOverflow",
	# "valid/runtimeErr/nullDereference",
	"valid/scope/ifNested1.wacc",
	"valid/scope/ifNested2.wacc",
	"valid/scope/indentationNotImportant.wacc",
	"valid/scope/intsAndKeywords.wacc",
	# "valid/scope/printAllTypes.wacc",
	"valid/scope/scope.wacc",
	"valid/scope/scopeBasic.wacc",
	"valid/scope/scopeIfRedefine.wacc",
	"valid/scope/scopeRedefine.wacc",
	"valid/scope/scopeSimpleRedefine.wacc",
	"valid/scope/scopeVars.wacc",
	"valid/scope/scopeWhileNested.wacc",
	"valid/scope/scopeWhileRedefine.wacc",
	"valid/scope/splitScope.wacc",
	"valid/sequence",
	"valid/variables",
	"valid/while"
]

################################################################################

from os import path as os_path, remove as os_remove, scandir, system
from pathlib import Path
import subprocess
from itertools import chain
import re
from sys import argv

IS_INTERPRETED = len(argv) > 0 and argv[1] == "-i"

global QEMU_NOT_FOUND
QEMU_NOT_FOUND = False

BLUE = "\033[1;34m"
GREEN = "\033[1;32m"
LIGHT_RED = "\033[1;31m"
RED = "\033[1;31m"
YELLOW = "\033[1;33m"
BOLD = "\033[1m"
END = "\033[0m"

PASSED = GREEN + "." + END
FAILED_COMPILE = RED + "C" + END
FAILED_EXIT = RED + "E" + END
FAILED_OUTPUT = RED + "O" + END
FAILED_TIMEOUT = RED + "T" + END
SKIPPED = YELLOW + "-" + END

failedTests = []

QEMU_TEST_PATHS = [Path("test/integration/" + path) for path in QEMU_TESTS]
TEST_PATHS = [Path("test/integration/" + path) for path in TESTS]
def shouldTestOutput(testPath):
	return any(runPath in testPath.parents or runPath == testPath for runPath in QEMU_TEST_PATHS)

wacc40exe = str(next(Path("./.stack-work/dist").rglob("build/WACC40-exe/WACC40-exe"), ""))

def integrationTests():
	print(PASSED, "passed")
	print(FAILED_COMPILE, "failed compilation")
	print(FAILED_EXIT, "wrong exit code")
	print(FAILED_OUTPUT, "wrong output")
	print(FAILED_TIMEOUT, "timed out after", TIMEOUT_DURATION, "secs")
	print(SKIPPED, "skipped")
	testSummary = ""
	skippedTests = totalTests = 0
	
	if not wacc40exe:
		print(RED + "\nWACC40-exe not found - did you", BOLD + "stack build" + END + RED + "?", END)
		exit(1)

	for testGroup in chain(scandir("./test/integration/invalid"), scandir("./test/integration/valid")):
		print(BOLD, BLUE, "\n", str(Path(testGroup))[17:], END)
		for waccFilename in Path(testGroup).rglob("*.wacc"):
			if not any(runPath in waccFilename.parents or runPath == waccFilename for runPath in TEST_PATHS):
				continue
			totalTests += 1
			if IS_INTERPRETED:
				expectedInput, expectedOutput, expectedExit = getWaccFileIO(waccFilename)
				basename = os_path.splitext(os_path.basename(waccFilename))[0]
				try:
					actualOutput, actualExit = getActualOutputInterpreted(waccFilename, expectedInput)
					if actualExit == expectedExit:
						testSummary += addTestResult(PASSED if "#runtime_error#" in expectedOutput or checkOutput(expectedOutput, actualOutput) else FAILED_OUTPUT, waccFilename, expectedOutput, actualOutput)
						if expectedExit != 0:
							continue
					else:
						testSummary += addTestResult(FAILED_EXIT, waccFilename, f"{LIGHT_RED}interpreter exit code: {expectedExit}{END}", f"{LIGHT_RED}interpreter exit code: {actualExit}{END}")
				except subprocess.TimeoutExpired:
					testSummary += addTestResult(FAILED_TIMEOUT, waccFilename, expectedOutput, f"{LIGHT_RED}Timed out after {TIMEOUT_DURATION} seconds{END}")
				continue
			# not interpreted
			result = subprocess.run(
				[wacc40exe, waccFilename],
				stdout=None if VIEW_STDOUT else subprocess.DEVNULL,
				stderr=None if VIEW_STDERR else subprocess.DEVNULL
			)

			expectedExit = getExpectedExit(waccFilename)
			actualExit = result.returncode
			if expectedExit != actualExit:
				testSummary += addTestResult(FAILED_COMPILE, waccFilename, f"{LIGHT_RED}./compile exit code: {expectedExit}{END}", f"{LIGHT_RED}./compile exit code: {actualExit}{END}")
				continue
			if expectedExit != 0:
				testSummary += addTestResult(PASSED)
				continue
			basename = os_path.splitext(os_path.basename(waccFilename))[0]

			if shouldTestOutput(waccFilename):
				expectedInput, expectedOutput, expectedExit = getWaccFileIO(waccFilename)
				try:
					actualOutput, actualExit = getActualOutput(basename, expectedInput)
					if actualExit == expectedExit:
						testSummary += addTestResult(PASSED if "#runtime_error#" in expectedOutput or checkOutput(expectedOutput, actualOutput) else FAILED_OUTPUT, waccFilename, expectedOutput, actualOutput)
					else:
						testSummary += addTestResult(FAILED_EXIT, waccFilename, f"{LIGHT_RED}qemu exit code: {expectedExit}{END}", f"{LIGHT_RED}qemu exit code: {actualExit}{END}")
				except subprocess.TimeoutExpired:
					testSummary += addTestResult(FAILED_TIMEOUT, waccFilename, expectedOutput, f"{LIGHT_RED}Timed out after {TIMEOUT_DURATION} seconds{END}")
			else:
				testSummary += addTestResult(SKIPPED)
				skippedTests += 1
			try:
				Path(f"./{basename}.s").unlink()
				# os_remove(f"./{basename}.s")
			except OSError as e:
				print(e)
				# if e.filename != f"./{basename}.s":
				# 	raise e
			try:
				os_remove(f"./{basename}")
			except FileNotFoundError as e:
				if e.filename != f"./{basename}":
					raise e


	print()
	for testname, expectedOutput, actualOutput in failedTests:
		print(RED, "--> Failed " + f"{testname}"[17:-5] + END)
		print(YELLOW, "\t", "Expected:", END)
		for line in expectedOutput.split("\n"):
			print("\t\t" + line)
		print(YELLOW, "\t", "Actual:", END)
		for line in actualOutput.split("\n"):
			print("\t\t" + line)
		# print()

	passedTests = totalTests - len(failedTests) - skippedTests
	if len(failedTests) > 0:
		print("\n" + PASSED, "passed")
		print(FAILED_COMPILE, "failed compilation")
		print(FAILED_EXIT, "wrong exit code")
		print(FAILED_OUTPUT, "wrong output")
		print(FAILED_TIMEOUT, "timed out after", TIMEOUT_DURATION, "secs")
		print(SKIPPED, "skipped")
		print(testSummary)

	print(BOLD, GREEN, "\n", passedTests, "passed," + RED, len(failedTests), "failed," + YELLOW, skippedTests, "skipped.", END)
	if QEMU_NOT_FOUND:
		print("qemu tests were run by refEmulate as command not found")

	exit(0 if len(failedTests) == 0 else 1)


def checkOutput(expectedOutput, actualOutput):
	return any(x in expectedOutput for x in [
		"#syntax_error#",
		"#semantic_error#",
		"#runtime_error#"
	]) or expectedOutput == re.sub(r"0x[\da-fA-F]+", "#addrs#", actualOutput)

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
	expectedExit = int(extract(waccCode, "# Exit:\n# ", "\n") or 0)
	expectedOutput = '\n'.join(line[2:] for line in waccFileOutputRaw.split("\n")) if waccFileOutputRaw else ""
	if expectedOutput and expectedOutput[0] == '\n':
		expectedOutput = expectedOutput[1:]
	return expectedInput, expectedOutput, expectedExit

def getActualOutput(basename, waccInput):
	try:
		result1 = subprocess.run(
			["arm-linux-gnueabi-gcc", "-o", basename, "-mcpu=arm1176jzf-s", "-mtune=arm1176jzf-s", basename + ".s"],
			stdout=None if VIEW_STDOUT else subprocess.DEVNULL,
			stderr=None if VIEW_STDERR else subprocess.DEVNULL
		)
		if result1.returncode != 0:
			return "", result1.returncode
		result2 = subprocess.run(
			["qemu-arm", "-L", "/usr/arm-linux-gnueabi/", basename],
			input=(waccInput or '') + '\n',
			capture_output=True,
			text=True,
			timeout=TIMEOUT_DURATION,
			universal_newlines=True
		)
		return result2.stdout, result2.returncode
	except FileNotFoundError as e:
		if e.filename == "arm-linux-gnueabi-gcc":
			global QEMU_NOT_FOUND
			QEMU_NOT_FOUND = True
			return runRefEmulator(basename + ".s", waccInput)
		else:
			raise e
	except UnicodeDecodeError as e:
		if VIEW_STDERR:
			print(e)
		return str(e), 0

def getActualOutputInterpreted(waccFilename, waccInput):
	try:
		if waccInput:
			waccInput = '\n'.join(waccInput.split(' '))
		result2 = subprocess.run(
			[wacc40exe, "-i", waccFilename],
			input=(waccInput or '\n'),
			capture_output=True,
			text=True,
			timeout=TIMEOUT_DURATION,
			universal_newlines=True,
		)
		if VIEW_STDOUT:
			print(result2.stdout)
		if VIEW_STDERR:
			print(result2.stderr)
		return result2.stdout, result2.returncode
	except FileNotFoundError as e:
		if e.filename == "arm-linux-gnueabi-gcc":
			global QEMU_NOT_FOUND
			QEMU_NOT_FOUND = True
			return runRefEmulator(basename + ".s", waccInput)
		else:
			raise e
	except UnicodeDecodeError as e:
		if VIEW_STDERR:
			print(e)
		return str(e), 0

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
	if assemblyOutput != "" and VIEW_STDOUT:
		print(assemblyOutput)
	output = extract(result.stdout, OUTPUT_DIVIDER, "\n" + LINE_DIVIDER)
	exitCode = extract(result.stdout, EXIT_CODE_TEXT, ".\n")
	return output, int(exitCode)

integrationTests()

system("rm -fr *.s")

