#!/usr/bin/env python

import glob
import os
import stats
import subprocess
from pylab import *
from matplotlib.backends.backend_pdf import PdfPages

SCRIPT_PATH = os.path.dirname(os.path.realpath(__file__)) + '/'
LC_PATH = SCRIPT_PATH + '../'
LC_EXEC = 'lazy-comp'

PDF_OUTPUT = SCRIPT_PATH + 'graphs.pdf'
BENCHMARKS_PATH = LC_PATH + 'benchmarks/*.scm'

os.chdir(LC_PATH) ## TODO pour test

files = glob.glob(BENCHMARKS_PATH)

#---------------------------------------------------------------------------
# Main

def main():
	# 1 - run benchmarks and parse compiler output
	keys,data = runparse()
	# # 2 - Draw all graphs
	drawGraphs(keys,data)

#---------------------------------------------------------------------------
# Run all benchmarks and parse outputs

def runparse():
	data = {}

	# Get keys
	first = files[0]
	keys = []

	for file in files:

		file_name  = os.path.basename(file)
		print(file_name + '...')

		output = subprocess.check_output([LC_PATH + LC_EXEC, file, '--stats']).decode("utf-8")

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
	# Gen pdf output file
	pdf = PdfPages(PDF_OUTPUT)
	# For each key
	for key in keys:
		# CSV, NYI
		if type(data[os.path.basename(files[0])][key]) == list:
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
	n = len(data)
	X = np.arange(n) # X set is [0, 1, ..., n-1]

	# Get Y values from data
	Y2 = []
	for f in files:
		Y2.extend([data[os.path.basename(f)][key]]);
	Y1 = np.array(Y2); # Y1 is a numpy array representation of Y1

	# Draw bars
	bar(X, +Y1, facecolor='#9999ff', edgecolor='white')

	# Hide X values
	axes = gca()
	axes.get_xaxis().set_visible(False)

	# Set Y limit
	l = len(str(max(Y2))) # number of digit of max value
	ylim(0,max(Y2)+pow(10,l-1)) # Y is from 0 to (max + 10^i-1)

	# Draw values for each bar
	for x,y in zip(X,Y1):
	    text(x+0.4, y+0.05, '%.2f' % y, ha='center', va= 'bottom')

	# Draw benchmark name
	for i in range(0,len(files)):
		text(X[i]+0.5, -0.0, os.path.basename(files[i])[:-4], rotation=25, ha='right', va='top')

	# Save to pdf
	pdf.savefig(fig)

#---------------------------------------------------------------------------

main()