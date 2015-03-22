#!/usr/bin/env python

import os
import subprocess

# Change working directory
os.chdir(os.path.dirname(os.path.realpath(__file__)))

LC = "../lazy-comp"
BPATH = "./benchmarks/"
COUNT = " --count-tests"
ALL   = " --all-tests"

files = os.listdir("../benchmarks/")
benchmarks = sorted(filter(lambda x: x[-4:] == ".scm", files))

NORMAL_R = {};
ALLTEST_R = {};

for benchmark in benchmarks:
	print(benchmark)

	output = subprocess.Popen([LC, BPATH + benchmark + COUNT], stdout=subprocess.PIPE).communicate()[0]
	nbtests = int(output.split('=')[1]);
	NORMAL_R[benchmark] = nbtests;

	output = subprocess.Popen([LC, BPATH + benchmark + COUNT + ALL], stdout=subprocess.PIPE).communicate()[0]
	nbtests = int(output.split('=')[1]);
	ALLTEST_R[benchmark] = nbtests;


hstr = "";
hstr = "Name"
hstr += " " * (12 - len("NAME"));

hstr += " " * (18 - len("Total tests"));
hstr += "Total tests"

hstr += " " * (20 - len("Executed tests"));
hstr += "Executed tests"

hstr += " " * (20 - len("% Removed"));
hstr += "% Removed"

print(hstr)

print("-" * 70)

PERC = 0.0;

for benchmark in benchmarks:
	bstr = "";
	bstr += benchmark.ljust(15);

	bstr += str(ALLTEST_R[benchmark]).rjust(15);

	bstr += str(NORMAL_R[benchmark]).rjust(20);

	p = 100 - ((NORMAL_R[benchmark] * 100.0) / ALLTEST_R[benchmark]);
	PERC += p;
	bstr += "{0:.2f}".format(p).rjust(20);
	
	print(bstr);


ave = PERC / len(benchmarks);
save = "{0:.2f}".format(ave)
print("-" * 70)
print("Average: " + save);
