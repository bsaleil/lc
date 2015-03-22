#!/usr/bin/env python2

import os
import subprocess

# Change working directory
os.chdir(os.path.dirname(os.path.realpath(__file__)).replace("/tools",""))

#-----------------------------------------------------------------------------

LC    = "./lazy-comp"
BPATH = "./benchmarks/"
COUNT = " --count-tests"
ALL   = " --all-tests"

# Get ordered benchmarks list
files = os.listdir(BPATH)
benchmarks = sorted(filter(lambda x: x[-4:] == ".scm", files))

EXECUTED_RESULT = {} # Hash table (benchmark,executed tests)
ALL_RESULT      = {} # Hash table (benchmark,total tests)

for benchmark in benchmarks:

	print(benchmark)

	# Count executed tests with optimizations
	output = subprocess.Popen([LC, BPATH + benchmark + COUNT], stdout=subprocess.PIPE).communicate()[0]
	nbtests = int(output.split('=')[1])
	EXECUTED_RESULT[benchmark] = nbtests

	# Count executed tests if none are removed
	output = subprocess.Popen([LC, BPATH + benchmark + COUNT + ALL], stdout=subprocess.PIPE).communicate()[0]
	nbtests = int(output.split('=')[1])
	ALL_RESULT[benchmark] = nbtests

#-----------------------------------------------------------------------------

# Print header
hstr = ""
hstr += "Name".ljust(15)
hstr += "Total tests".rjust(15)
hstr += "Executed tests".rjust(20)
hstr += "% Removed".rjust(20)
print(hstr)
print("-" * 70)

# % sum of all benchmarks
sum_percentage = 0.0

# Print results
for benchmark in benchmarks:

	bstr = ""
	bstr += benchmark.ljust(15)                  # Benchmark name
	bstr += str(ALL_RESULT[benchmark]).rjust(15) # Benchmark # total tests
	bstr += str(EXECUTED_RESULT[benchmark]).rjust(20)  # Benchmark # executed tests
	percentage = 100 - ((EXECUTED_RESULT[benchmark] * 100.0) / ALL_RESULT[benchmark])
	bstr += "{0:.2f}".format(percentage).rjust(20)     # % of executed tests
	sum_percentage += percentage
	
	print(bstr)

# Compute and print average
average = sum_percentage / len(benchmarks)
straverage = "{0:.2f}".format(average)
print("-" * 70)
print("Average: " + straverage)

#-----------------------------------------------------------------------------
