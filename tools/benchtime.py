#!/usr/bin/env python

import os
import glob
import subprocess
from pylab import *
from matplotlib.backends.backend_pdf import PdfPages

#### TODO: ajouter edgecolor 'none' dans graphs.py (plus beau)
#### TODO: sort execs

help = """
benchtime.py - Generate graph with benchmarks execution times

Use:
	benchtime.py [OPTION...]

Options:
	-h,--help
		Print this help.
	--num-exec
		Set the number of executions for each benchmark. (Default is 5)
	--draw-exec
		Set the execution to draw. (Default is 2)
                1 is the execution including compilation and execution time
                2 is the execution including only execution time

Example:

	benchtime.py
		Draw graph using default parameters

	benchtime.py --num-exec 10 --draw-exec 1
		Draw graph for first execution using 10 executions for each benchmark
"""

#----------------------------------------------------------------------------------------------------

SCRIPT_PATH = os.path.dirname(os.path.realpath(__file__)) + '/' # Current script path
LC_PATH     = SCRIPT_PATH + '../'                               # Compiler path
LC_EXEC     = 'lazy-comp'                                       # Compiler exec name
BENCH_PATH  = LC_PATH + 'benchmarks/*.scm'                      # Benchmarks path
PDF_OUTPUT  = SCRIPT_PATH + 'time.pdf'                        # PDF output file

#----------------------------------------------------------------------------------------------------
# Script params

EXEC_NUMBER = 5 # Number of executions for each benchmark
DRAW_EXEC   = 2 # Execution to draw. 1 is execution including compilation tim and 2 is execution including only execution time

if '-h' in sys.argv or '--help' in sys.argv:
    print(help);
    sys.exit();
if '--num-exec' in sys.argv:
    nu = int(sys.argv[sys.argv.index('--num-exec')+1])
    EXEC_NUMBER = nu;

if '--draw-exec' in sys.argv:
    ex = int(sys.argv[sys.argv.index('--draw-exec')+1]);
    if (ex != 1) and (ex != 2):
        print(help);
        sys.exit();
    DRAW_EXEC = ex;

print("\nRun script with:")
print("   Number of executions: " + str(EXEC_NUMBER))
print("   Execution to draw:    " + str(DRAW_EXEC))

#----------------------------------------------------------------------------------------------------

# Set current working directory to compiler path
os.chdir(LC_PATH)
# Get all benchmarks full path sorted by name
files = sorted(glob.glob(BENCH_PATH))

#----------------------------------------------------------------------------------------------------
# Exectue EXEC_NUMBER times the benchmark 'bench' with options 'opts'
# Remove min, max and return average of remaining times

def getExecTime(bench,opts):

    options = [LC_PATH + LC_EXEC, bench]
    options.extend(opts);

    currTimes = [];
    for i in range(0,EXEC_NUMBER):
        print(' ' + str(i+1), end='')
        sys.stdout.flush()
        output = subprocess.check_output(options).decode("utf-8") # Exec LC
        times = output.split('\n');
        cyclesSndExec = int(times[(DRAW_EXEC - 1)*2].split(':')[1]); # Get cycles number of DRAW_EXECth execution
        currTimes.append(cyclesSndExec);

    print('')
    currTimes.remove(max(currTimes))
    currTimes.remove(min(currTimes))

    return sum(currTimes) / float(EXEC_NUMBER-2);

#----------------------------------------------------------------------------------------------------
# Draw graph with exec times in cycles

def drawGraph(times,sortedKeys):

	# Open pdf output file
	pdf = PdfPages(PDF_OUTPUT)

	fig = plt.figure("TIMES",figsize=(22,7))
	plt.title('Exec ' + str(DRAW_EXEC))

	xvalsA = [] # Left bar
	xvalsB = [] # Right bar
	yvalsA = [] # Times with versioning only
	yvalsB = [] # Times with versioning and multiple entry points
	keys   = [] # Benchmark names

	names = sortedKeys;
	i = 0;
	for name in names:
	    keys.append(name);
	    xvalsA.append(i);
	    xvalsB.append(i+1);
	    i+=3;
	    yvalsA.append(times[name][1]);
	    yvalsB.append(times[name][2]);

	plt.bar(xvalsA, yvalsA, 1, facecolor="#666666", edgecolor='none',label='Versioning')
	plt.bar(xvalsB, yvalsB, 1, facecolor="#BBBBBB", edgecolor='none',label='Interprocedural versioning')

	axes = plt.gca()
	axes.get_xaxis().set_visible(False)

	for i in range(0,len(keys)):
	    plt.text(xvalsA[i]+1,-0.01,keys[i],ha='center',va='top',rotation=90,size=18)

	# Draw legend
	box = axes.get_position()
	axes.set_position([box.x0, box.y0 + box.height * 0.25, box.width, box.height * 0.75])
	plt.legend(loc='upper center', bbox_to_anchor=(0., 0., 1., -0.33), prop={'size':19}, ncol=2, mode='expand', borderaxespad=0.)

	#plt.ylim(0,2);
	#plt.show()
	pdf.savefig(fig);
	pdf.close();

#----------------------------------------------------------------------------------------------------
# Return times for all benchmarks

def getAllTimes():

    benchTimes = {}

    i = 0;
    for file in files:

        i+=1
        print('\n(' + str(i) + '/' + str(len(files)) + ')', end='')

        print("-- Exec " + file.split('/')[-1] + "...")
        # Time without optimisations
        print("   No Optimization:", end='')
        timeNoOpt = getExecTime(file,['--max-versions 0','--disable-entry-points','--time']);
        # Time using only versioning
        print("   Versioning     :", end='')
        timeVers  = getExecTime(file,['--disable-entry-points','--time'])
        # Time using versioning and multiple entry points
        print("   Versioning & EP:", end='')
        timeVersEP = getExecTime(file,['--time'])

        benchTimes[file.split('/')[-1][:-4]] = [1.0,timeVers/timeNoOpt,timeVersEP/timeNoOpt];

    return benchTimes;

#----------------------------------------------------------------------------------------------------
# Main

def sort_key(el):
	return times[el][DRAW_EXEC];

times = getAllTimes();
sortedKeys = sorted(times, key = sort_key);
drawGraph(times,sortedKeys);

print('DONE!')
