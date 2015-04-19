#!/usr/bin/env python

# Read stats output from compiler and print python table representation

import sys
import io

CSV_INDICATOR  = '--' 
STAT_SEPARATOR = ':'
CSV_SEPARATOR  = ';'
	
#---------------------------------------------------------------------------
# Utils

def num(s):
    try:
        return int(s)
    except ValueError:
        return float(s)

#---------------------------------------------------------------------------
# Output parser

def readStat(stream,data,line):
	stat = line.split(STAT_SEPARATOR)
	key = stat[0].strip()
	val = num(stat[1].strip())
	# Store key/value in global data
	data[key] = val
	line = stream.readline()
	return line

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

def parseOutput(output):
	# Data for this benchmark
	data = {}
	# Stream
	stream = io.StringIO(output)
	# Parse...
	line = stream.readline()
	while line:
		# CSV table
		if line.startswith(CSV_INDICATOR):
			line = readCSV(stream,data)
		# Key/Value line
		else:
			line = readStat(stream,data,line)
	return data

#--------------------------------------------------------------------------