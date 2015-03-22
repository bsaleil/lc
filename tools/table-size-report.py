#!/usr/bin/env python2

import os
import subprocess

# Change working directory
os.chdir(os.path.dirname(os.path.realpath(__file__)).replace("/tools",""))

#-----------------------------------------------------------------------------

LC         = "./lazy-comp"
BPATH      = "./benchmarks/"
TABLE_SIZE = " --print-ccsize"

# Get ordered benchmarks list
files = os.listdir(BPATH)
benchmarks = sorted(filter(lambda x: x[-4:] == ".scm", files))

SIZE_RESULT = {} # Hash table (benchmark,table size)

for benchmark in benchmarks:

	print(benchmark)

	# Print table size of this benchmark
	output = subprocess.Popen([LC, BPATH + benchmark + TABLE_SIZE], stdout=subprocess.PIPE).communicate()[0]
	size = int(output.split('=')[1])
	SIZE_RESULT[benchmark] = size

#-----------------------------------------------------------------------------


# Print header
hstr = ""
hstr += "Name".ljust(15)
hstr += "Table size".rjust(15)
print(hstr)
print("-" * 30)

# Sum of size for all benchmarks
sum_size = 0

# Print results
for benchmark in benchmarks:

	bstr = ""
	bstr += benchmark.ljust(15)
	bstr += str(SIZE_RESULT[benchmark]).rjust(15)
	sum_size += SIZE_RESULT[benchmark]

	print(bstr);

# Compute and print average
average = sum_size / len(benchmarks)
print("-" * 30)
print("Average: " + str(average))

#-----------------------------------------------------------------------------
