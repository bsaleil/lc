#!/usr/bin/env python2

import os
import subprocess

# Change working directory
os.chdir(os.path.dirname(os.path.realpath(__file__)).replace("/tools",""))

#-----------------------------------------------------------------------------

LC         = "./lazy-comp"
BPATH      = "./benchmarks/"
TABLE_SIZE = " --count-closures"

# Get ordered benchmarks list
files = os.listdir(BPATH)
benchmarks = sorted(filter(lambda x: x[-4:] == ".scm", files))

NB_RESULT = {} # Hash table (benchmark,min nb versions)

for benchmark in benchmarks:

	print(benchmark)

	# Print table size of this benchmark
	output = subprocess.Popen([LC, BPATH + benchmark + TABLE_SIZE], stdout=subprocess.PIPE).communicate()[0]
	nb = int(output.split('=')[1])
	NB_RESULT[benchmark] = nb;

#-----------------------------------------------------------------------------

# Print header
hstr = ""
hstr += "Name".ljust(15)
hstr += "NB closures".rjust(15)
print(hstr)
print("-" * 50)

# Print results
for benchmark in benchmarks:

	bstr = ""
	bstr += benchmark.ljust(15)
	bstr += str(NB_RESULT[benchmark]).rjust(15)

	print(bstr)

#-----------------------------------------------------------------------------
