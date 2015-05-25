#!/usr/bin/env python

# Execute compiler with stats option for all benchmarks
# Parse output
# Draw graphs

help = """
graphs.py - Generate graphs from compiler output

Use:
	graphs.py [OPTION...]

Options:
	-h,--help
		Print this help.
	--drawall
		Draw all graphs. By default the script let the user choose the information to draw.
	--stdexec
		Use standard execution. Same as --exec="Standard;"?
	--exec="DESCRIPTION;COMPILER_OPTION1 COMPILER_OPTION2 ..."
		Add execution with given compiler options. All given executions are drawn

Example:
	
	graphs.py --exec="Standard exec;" --exec="With all tests;--all-tests" --drawall
		Draw all graphs for both executions (Standard, and with all-tests option).

	graphs.py --stdexec
		Let the user interactively choose the information to draw from only standard execution.
"""

import sys
import io
import glob
import os
import subprocess
from pylab import *
from copy import deepcopy
from matplotlib.backends.backend_pdf import PdfPages

# Constants
SCRIPT_PATH = os.path.dirname(os.path.realpath(__file__)) + '/' # Current script path
LC_PATH     = SCRIPT_PATH + '../'                               # Compiler path
LC_EXEC     = 'lazy-comp'                                       # Compiler exec name
PDF_OUTPUT  = SCRIPT_PATH + 'graphs.pdf'                        # PDF output file
BENCH_PATH  = LC_PATH + 'benchmarks/*.scm'                      # Benchmarks path
#BAR_COLORS  = ["#444444","#666666","#888888","#AAAAAA","#CCCCCC","#EEEEEEn"]         # Bar colors
BAR_COLORS = ["#222222", "#666666", "#AAAAAA", "#EEEEEE"] # Paper sw15

# Parser constants, must match compiler --stats output
CSV_INDICATOR  = '--' 
STAT_SEPARATOR = ':'
CSV_SEPARATOR  = ';'

# Options
DRAW_ALL = '--drawall' # Draw all graphs
STD_EXEC = '--stdexec' # Add standard execution to executions list
REF_EXEC = '--refexec' # Set reference execution for scale
SORT_EXEC = '--sortexec' # Sort

OPT_REF = False
OPT_SORT = False

# Globals
execs     = {}
lexecs    = []
printhelp = False

# Set current working directory to compiler path
os.chdir(LC_PATH)

# Get all benchmarks full path sorted by name
files = sorted(glob.glob(BENCH_PATH))

#-------------------------------------------------------------------------------------
# Utils

def num(s):
    try:
        return int(s)
    except ValueError:
        return float(s)

def WARNING(s):
	print('WARNING: ' + s)

#-------------------------------------------------------------------------------------
# Main

def setargs():
	global printhelp
	global OPT_REF
	global OPT_SORT
	if '-h' in sys.argv or '--help' in sys.argv:
		printhelp = True
	if STD_EXEC in sys.argv:
		execs['Standard'] = ''
	if REF_EXEC in sys.argv:
		OPT_REF = sys.argv[sys.argv.index(REF_EXEC)+1]
	if SORT_EXEC in sys.argv:
		OPT_SORT = sys.argv[sys.argv.index(SORT_EXEC)+1]

	for arg in sys.argv:
		if arg.startswith('--exec='):
			pair = arg[7:].split(';')
			name = pair[0]
			lcargs = pair[1].split()
			execs[name] = lcargs
			lexecs.append(name)

def go():
	if printhelp:
		print(help)
	else:
		# 1 - run benchmarks and parse compiler output
		benchs_data = {}
		keys = []
		for ex in execs:
			ks,data = runparse(execs[ex]) # TODO : donner arguments
			if keys == []:
				keys = ks
			else:
				if len(ks) != len(keys):
					raise Exception("Error")
			benchs_data[ex] = data

		# 2 - Draw all graphs
		drawGraphs(keys,benchs_data)
		print('Done!')	

# Run compiler with 'opts', parse output and return keys and data
def runparse(opts):
	print("Running with options: '" + ' '.join(opts) + "'")
	data = {}

	# Get keys
	first = files[0]
	keys = []

	for file in files:

		file_name  = os.path.basename(file)
		print(file_name + '...')

		options = [LC_PATH + LC_EXEC, file, '--stats']
		options.extend(opts) # TODO : renommer 'options'
		output = subprocess.check_output(options).decode("utf-8")

		bench_data = parseOutput(output)
		
		data[file_name] = bench_data

		# Get keys on first result
		if file == first:
			for key in bench_data:
				keys.append(key)

	return keys,data

#-------------------------------------------------------------------------------------
# Parser: Read stats output from compiler and return python table representation

# Read 'KEY:VALUE' stat
def readStat(stream,data,line):
	stat = line.split(STAT_SEPARATOR)
	key = stat[0].strip()
	val = num(stat[1].strip())
	# Store key/value in global data
	data[key] = val
	line = stream.readline()
	return line

# Read CSV stat
def readCSV(stream,data):
	csv = []
	# Consume CSV indicator line
	line = stream.readline()
	# Read table title
	title = line.strip()
	line = stream.readline()
	# Read table header
	header = line.split(CSV_SEPARATOR)
	for el in header:
		csv.append([el.strip()])
	# Read CSV data
	line = stream.readline()
	while not line.startswith(CSV_INDICATOR):
		linecsv = line.split(CSV_SEPARATOR)
		for i in range(0,len(linecsv)):
			csv[i].extend([num(linecsv[i].strip())]) ## THIS IS NOT EFFICIENT (for large CSV outputs)
		line = stream.readline()
	# Store key/value (title/csv) in global data
	data[title] = csv
	# Consume CSV indicator line
	line = stream.readline()
	return line

# Return python table from compiler 'output'
def parseOutput(output):
	# Data for this benchmark
	data = {}
	# Stream
	stream = io.StringIO(output)
	# Parse
	line = stream.readline()
	while line:
		# CSV table
		if line.startswith(CSV_INDICATOR):
			line = readCSV(stream,data)
		# Key/Value line
		else:
			line = readStat(stream,data,line)
	return data

#-------------------------------------------------------------------------------------
# Draw

# Draw all graphs associated to keys using benchs_data
# benchs_data contains all information for all benchmarks for all executions
# ex. benchs_data['Standard']['array1.scm']['Closures'] to get the number of
#     closures created for benchmark array1.scm using standard exec
def drawGraphs(keys,benchs_data):

	# Let user choose the graph to draw (-1 or empty for all graphs)
	if not DRAW_ALL in sys.argv:
		sortedKeys = sorted(keys)
		print('Keys:')
		print('-1: ALL')
		for i in range(0,len(sortedKeys)):
			print(' ' + str(i) + ': ' + sortedKeys[i])
		inp = input('Key to draw (all) > ')
		if not inp == '':
			choice = num(inp)
			if choice >= 0:
				keys = [sortedKeys[choice]]

	firstExec = list(benchs_data.keys())[0]
	firstBenchmark = os.path.basename(files[0])

	# Gen pdf output file
	pdf = PdfPages(PDF_OUTPUT)

	# For each key
	for key in keys:
		# CSV, NYI
		if type(benchs_data[firstExec][firstBenchmark][key]) == list:
			drawCSV(pdf,key,benchs_data)
		# Key/Value, draw graph
		else:
			print("Drawing '" + key + "'...")
			drawKeyValueGraph(pdf,key,benchs_data)

	pdf.close()

## This is a specific implementation for #stubs/#versions
## TODO: Do something generic !
def drawCSV(pdf,key,benchs_data):
	fig = plt.figure(key)
	title = key
	res = {}

	for execution in benchs_data:
		for bench in benchs_data[execution]:
			for data in benchs_data[execution][bench][key]:
			   if data[0] == '#stubs':
			     for i in range(0,len(data)-1):
			     	index = i+1
			     	numvers = i
			     	if (numvers >= 5):
			     		numvers = -1
			     	if (numvers in res):
			     		res[numvers] += data[index]
			     	else:
			     		res[numvers] = data[index]

	xvals = []
	yvals = []
	labels = []

	keys = sorted(res.keys())

	for key in keys:
		if key != 0 and key != -1:
			xvals.append(key)
			yvals.append(res[key])
			labels.append(key)

	xvals.append(len(xvals)+1)
	yvals.append(res[-1])
	labels.append('>=5')

	sum = 0
	for val in yvals:
		sum += val
	for i in range(0,len(yvals)):
		p = (yvals[i] * 100) / sum
		yvals[i] = p

	plt.title(title + ' (total=' + str(sum) + ')')

	X = np.array(xvals)
	Y = np.array(yvals)

	bar(X, +Y, 1, facecolor=BAR_COLORS[0], edgecolor='white', label=key, zorder=10)

	axes = gca()
	axes.get_xaxis().set_visible(False)

	# Draw grid
	axes = gca()
	axes.grid(True, zorder=1, color="#707070")
	axes.set_axisbelow(True) # Keep grid under the axes

	for i in range(0,len(labels)):
		text(X[i]+0.25, -0.0, labels[i], ha='right', va='top')

	# print(xvals)
	# print(yvals)
	# print(labels)
	# print(res)
	pdf.savefig(fig)

# Draw graph for given key
# Y: values for this key
# X: benchmarks
def drawKeyValueGraph(pdf,key,benchs_data):
	fig = plt.figure(key,figsize=(22,7))
	#plt.title(key)

	exec_ref = ''

	# Number of benchmarks
	firstExec = list(benchs_data.keys())[0]
	n = len(benchs_data[firstExec])
	X = np.arange(n) # X set is [0, 1, ..., n-1]

	Ys = {}
	# pour chaque executions
	for d in benchs_data:
		Y = []
		# pour chaque benchmark
		for f in files:
			Y.extend([benchs_data[d][os.path.basename(f)][key]])
		# Transforme en tableau numpy
		Y = np.array(Y)
		Ys[d] = Y
	
	width = 1 / (len(Ys)+1)

	#----------
	# TODO: move to external fn
	# Use a reference execution. All values for this exec are 100%
	# Values for others executions are computed from this reference exec
	if OPT_REF:
		exec_ref = OPT_REF # Reference execution (100%)
		Y2 = deepcopy(Ys)      # Deep copy of Y values
		# Set all references to 100
		for v in range(0,len(Y2[exec_ref])):
			Y2[exec_ref][v] = '100'
		# For each exec which is not ref exec
		candraw = True # TODO : rename
		for ex in Y2:
			if ex != exec_ref:
				for i in range(0,len(Y2[ex])):
					ref = Ys[exec_ref][i]
					cur = Ys[ex][i]
					# We can't compute %, warning and stop
					if ref == 0:
						WARNING("Can't draw '" + key + "' using a reference execution.")
						return
					# Compute % and set
					else:
						Y2[ex][i] = (cur*100)/ref
		# Y2 are the new values to draw
		Ys = Y2
	#----------

	fileList = files
	Yvals = Ys
	
	# Sort Y values by a given execution
	if OPT_SORT:
		fileList,Yvals = sortByExecution(Yvals,OPT_SORT)

	# Draw grid
	axes = gca()
	axes.grid(True, zorder=1, color="#707070")
	axes.set_axisbelow(True) # Keep grid under the axes

	i = 0

	# TODO: add to --help: the script draws the exec bar in order
	for key in lexecs:
		if key != exec_ref:
			Y = Yvals[key]
			color = BAR_COLORS[i]
			bar(X+(i*width), +Y, width, facecolor=color, edgecolor='white', label=key, zorder=10)
			i += 1

	# Hide X values
	axes.get_xaxis().set_visible(False)

	plt.tick_params(axis='both', which='minor', labelsize=19)

	plt.yticks(fontsize=19)

	# # Set Y limit
	#l = len(str(max(Y2))) # number of digit of max value
	#ylim(0,max(Y2)+pow(10,l-1)) # Y is from 0 to (max + 10^i-1)
	# # Draw values for each bar
	# for x,y in zip(X,Y1):
	#     text(x+0.4, y+0.05, '%.2f' % y, ha='center', va= 'bottom')
	ylim(0,100);

	# Draw benchmark name
	for i in range(0,len(fileList)):
		text(X[i]+0.28, -3, os.path.basename(fileList[i])[:-4], rotation=90, ha='center', va='top', size=19)


	# Legend:
	# Shrink by 10% on the bottom
	box = axes.get_position()
	axes.set_position([box.x0, box.y0 + box.height * 0.25, box.width, box.height * 0.75])
	# Put a legend below axis
	legend(loc='upper center', bbox_to_anchor=(0., 0., 1., -0.33), prop={'size':19}, ncol=len(lexecs)-1, mode='expand', borderaxespad=0.)

	# Save to pdf
	pdf.savefig(fig)

#-------------------------------------------------------------------------------------
# Manage Y values

# Sort Y values by values from a specific execution
def sortByExecution(Ys,execref):

	# Pseudo-decorate: Change data layout to allow the useof sort()
	decorated = []
	for fileIndex in range(0,len(files)):
		r = [] # List of results for current file
		for execution in Ys:
			r.extend([execution,Ys[execution][fileIndex]])
		r.append(files[fileIndex])
		decorated.append(r)

	# Sort
	i = decorated[0].index(execref)
	decorated = sorted(decorated,key=lambda el: el[i+1])
	# Pseudo-undecorate: Restore previous layout with sorted data
	undecorated = {}
	ordered_files = []
	i = 0;
	while not decorated[0][i] in files:
		execution = decorated[0][i]
		vals = []
		# For each data associated to file
		for el in decorated:
			vals.append(el[i+1])
			filepath = el[len(el)-1]
			if not filepath in ordered_files:
				ordered_files.append(filepath)
		undecorated[execution] = np.asarray(vals);
		i+=2

	return(ordered_files,undecorated)

#-------------------------------------------------------------------------------------

setargs()
go()