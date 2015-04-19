#!/usr/bin/env python

# Execute compiler with stats option for all benchmarks
# Parse output
# Draw graph for each information

#./graphs.py --exec="Normale;" --exec="Tous les tests;--all-tests"


import glob
import os
import stats
import subprocess
from pylab import *
from matplotlib.backends.backend_pdf import PdfPages

# Current script path
SCRIPT_PATH = os.path.dirname(os.path.realpath(__file__)) + '/'
# Compiler path
LC_PATH = SCRIPT_PATH + '../'
# Compiler exec name
LC_EXEC = 'lazy-comp'
LC_OPTS = ''
# PDF output file
PDF_OUTPUT = SCRIPT_PATH + 'graphs.pdf'
# Benchmarks path
BENCHMARKS_PATH = LC_PATH + 'benchmarks/*.scm'
# Bar colors
BAR_COLORS=["#444444","#666666","#888888","#AAAAAA"]
# Set current working directory to compiler path
os.chdir(LC_PATH)

# Options
DRAW_ALL = '--drawall' # Draw all graphs

# Get all benchmarks path
files = sorted(glob.glob(BENCHMARKS_PATH)) ## TODO trié par nom 
args  = sys.argv

execs = {};
for arg in args:
	if arg.startswith('--exec='):
		pair = arg[7:].split(';')
		name = pair[0]
		lcargs = pair[1].split()
		execs[name] = lcargs;
# execs['normal'] = '';
# execs['popopo'] = '--all-tests';
# execs['popopos'] = '--all-tests';
# execs['popopso'] = '';

#---------------------------------------------------------------------------
# Main

def main():
	# 1 - run benchmarks and parse compiler output
	datas = {}
	keys = []
	for ex in execs:
		ks,data = runparse(execs[ex]) # TODO : donner arguments
		if keys == []:
			keys = ks
		else:
			if len(ks) != len(keys):
				raise Exception("Error")
		datas[ex] = data

	# 2 - Draw all graphs
	drawGraphs(keys,datas)

#---------------------------------------------------------------------------
# Run all benchmarks and parse outputs

def runparse(opts):
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

		bench_data = stats.parseOutput(output)
		
		data[file_name] = bench_data

		# Get keys on first result
		if file == first:
			for key in bench_data:
				keys.append(key)

	print("Run/Parse done !")
	return keys,data

#---------------------------------------------------------------------------
# Draw graphs

def drawGraphs(keys,data):
	
	# Let user choose the graph to draw (-1 or empty for all graphs)
	if not DRAW_ALL in args:
		sortedKeys = sorted(keys)
		print('Keys:')
		print('-1: ALL')
		for i in range(0,len(sortedKeys)):
			print(' ' + str(i) + ': ' + sortedKeys[i])
		inp = input('Key to draw (all) > ')
		if not inp == '':
			choice = stats.num(inp)
			if choice >= 0:
				keys = [sortedKeys[choice]]

	firstExec = list(data.keys())[0]
	firstBenchmark = os.path.basename(files[0])
	# Gen pdf output file
	pdf = PdfPages(PDF_OUTPUT)

	# For each key
	for key in keys:
		# CSV, NYI
		if type(data[firstExec][firstBenchmark][key]) == list:
			None # NYI
		# Key/Value, draw graph
		else:
			print("Drawing '" + key + "'...")
			drawKeyValueGraph(pdf,key,data)

	print("Drawing done !")
	pdf.close()

# Draw graph for given key
# Y: values of this key
# X: benchmarks
def drawKeyValueGraph(pdf,key,data):
	fig = plt.figure(key)
	plt.title(key)

	# Number of benchmarks
	firstExec = list(data.keys())[0]
	n = len(data[firstExec])
	X = np.arange(n) # X set is [0, 1, ..., n-1]

	Ys = {}
	# pour chaque executions
	for d in data:
		Y = []
		# pour chaque benchmark
		for f in files:
			Y.extend([data[d][os.path.basename(f)][key]]);
		# Transforme en tableau numpy
		Y = np.array(Y);
		Ys[d] = Y;
	
	width = 1 / (len(Ys)+1)

	i = 0
	for mode in Ys: # todo : renommer mode
		Y = Ys[mode]
		bar(X+(i*width), +Y, width, facecolor=BAR_COLORS[i], edgecolor='white') # TODO : changer couleur
		i += 1

	# Hide X values
	axes = gca()
	axes.get_xaxis().set_visible(False)

	# # Set Y limit
	# l = len(str(max(Y2))) # number of digit of max value
	# ylim(0,max(Y2)+pow(10,l-1)) # Y is from 0 to (max + 10^i-1)

	# # Draw values for each bar
	# for x,y in zip(X,Y1):
	#     text(x+0.4, y+0.05, '%.2f' % y, ha='center', va= 'bottom')

	# Draw benchmark name
	for i in range(0,len(files)):
		text(X[i]+0.5, -0.0, os.path.basename(files[i])[:-4], rotation=25, ha='right', va='top')

	# Save to pdf
	pdf.savefig(fig)

#---------------------------------------------------------------------------

main()