#!/usr/bin/env python2

import os
import subprocess

# Change working directory
os.chdir(os.path.dirname(os.path.realpath(__file__)).replace("/tools",""))

#-----------------------------------------------------------------------------

LC         = "./lazy-comp"
BPATH      = "./benchmarks/"
TABLE_SIZE = " --print-versions"

# Get ordered benchmarks list
files = os.listdir(BPATH)
benchmarks = sorted(filter(lambda x: x[-4:] == ".scm", files))

MIN_RESULT = {} # Hash table (benchmark,min nb versions)
MAX_RESULT = {} # Hash table (benchmark,max nb versions)

for benchmark in benchmarks:

	print(benchmark)

	# Print table size of this benchmark
	output = subprocess.Popen([LC, BPATH + benchmark + TABLE_SIZE], stdout=subprocess.PIPE).communicate()[0]
	r = output.split('Min:')[1].split('Max:')
	MIN_RESULT[benchmark] = int(r[0])
	MAX_RESULT[benchmark] = int(r[1])

#-----------------------------------------------------------------------------


# Print header
hstr = ""
hstr += "Name".ljust(15)
hstr += "Min".rjust(15)
hstr += "Max".rjust(20)
print(hstr)
print("-" * 50)

# Sum of versions
sum_min = 0
sum_max = 0

# Print results
for benchmark in benchmarks:

	bstr = ""
	bstr += benchmark.ljust(15)
	bstr += str(MIN_RESULT[benchmark]).rjust(15)
	sum_min += MIN_RESULT[benchmark]

	bstr += str(MAX_RESULT[benchmark]).rjust(20)
	sum_max += MAX_RESULT[benchmark]

	print(bstr)

# Compute and print average
min_average = sum_min / len(benchmarks)
max_average = sum_max / len(benchmarks)
print("-" * 50)
print("Average min: " + str(min_average))
print("Average max: " + str(max_average))

#-----------------------------------------------------------------------------
