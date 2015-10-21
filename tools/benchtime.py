#!/usr/bin/python3

import sys # TODO
import os
import glob
import subprocess
from pylab import *
from matplotlib.backends.backend_pdf import PdfPages

# -------------------------
# Constants
# -------------------------

SCRIPT_PATH = os.path.dirname(os.path.realpath(__file__)) + '/' # Current script path
LC_PATH     = SCRIPT_PATH + '../'                               # Compiler path
LC_EXEC     = 'lazy-comp'                                       # Compiler exec name
PDF_OUTPUT  = SCRIPT_PATH + 'times.pdf'                         # PDF output file
BENCH_PATH  = LC_PATH + 'benchmarks/*.scm'                      # Benchmarks path
BAR_COLORS  = ["#222222", "#666666", "#AAAAAA", "#EEEEEE"]      # Bar colors
ITERS       = 10                                                # Number of iterations >=4 (we remove first, min and max)

# -------------------------
# Config
# -------------------------

# Change to lc directory
os.chdir(LC_PATH)

# Get all benchmarks full path sorted by name
files = sorted(glob.glob(BENCH_PATH))

# -------------------------
# Exec benchmarks
# and get times
# -------------------------

# Execute ITERS times the file with given options
# Remove first, min and max times and return the sum
def getTime(file,options):
    opts = [LC_PATH + LC_EXEC, file, '--time']
    opts.extend(options)
    times = []
    for i in range(0,ITERS):
        output = subprocess.check_output(opts).decode("utf-8")
        times.append( float(output.split(':')[1].strip()))
    # Remove first,min,max
    times.remove(times[0])
    times.remove(min(times))
    times.remove(max(times))
    return sum(times)

TIME = []

idx = 0
for file in files:
    idx+=1
    print('(' + str(idx) + '/' + str(len(files)) + ') ' + file);

    # Exec with versioning only
    print('\t* Versioning only...')
    time_v = getTime(file,['--disable-entry-points', '--disable-return-points']);

    # Exec with versioning and entry points
    print('\t* Versioning + entry points...')
    time_ve = getTime(file,['--disable-return-points']);

    # Exec with versioning only
    print('\t* Versioning + return points...')
    time_vr = getTime(file,['--disable-entry-points']);

    # Exec with versioning only
    print('\t* Versioning + entry points + return points...')
    time_ver = getTime(file,[]);

    TIME.append([file,time_v,time_ve,time_vr,time_ver])

# -------------------------
# Draw graph
# -------------------------

# Draw bars on graph with times of times_p at index time_idx (which is one of ve, vr, ver)
# This bar will use the given color and label
def drawBar(times_p,time_idx,color_idx,label):
    Y = list(map(lambda x: x[time_idx], times_p))
    bar(X, Y, bar_width, facecolor=BAR_COLORS[color_idx], edgecolor='white', label=label, zorder=10)

print('Draw graph...')

# Graph config
nb_items = len(files) + 1                     # Number of times to draw (+1 for arithmetic mean)
bar_width = 0.8                               # Define widht of a single bar
matplotlib.rcParams.update({'font.size': 19}) # Set font size of all elements of the graph
fig = plt.figure('',figsize=(22,9))           # Create new figure
plt.title('Execution time')                   # Set title
gca().get_xaxis().set_visible(False)          # Hide x values

ylim(0,200)                                   # Set y scale from 0 to 200
fig.subplots_adjust(bottom=0.4)               # Give more space at bottom for benchmark names
plot([0,nb_items*4],[100,100], color="#CCCCCC", linewidth=1) # Draw y=100 line

# Convert times to % times
TIMEP = []
for t in TIME:
    file = t[0]
    time_v = t[1]
    time_ve = t[2]
    time_vr = t[3]
    time_ver = t[4]
    # Compute % values relative to time with versioning only
    timep_v   = 100
    timep_ve  = (100*time_ve)  / time_v
    timep_vr  = (100*time_vr)  / time_v
    timep_ver = (100*time_ver) / time_v

    TIMEP.append([file,timep_v,timep_ve,timep_vr,timep_ver])

# Sort by timep_ver
TIMEP.sort(key=lambda x: x[4])

# Add arithmetic mean values
name = 'arith. mean'
timep_v   = 100
timep_ve  = sum(list(map(lambda x: x[2], TIMEP)))/len(files)
timep_vr  = sum(list(map(lambda x: x[3], TIMEP)))/len(files)
timep_ver = sum(list(map(lambda x: x[4], TIMEP)))/len(files)
TIMEP.append([name,timep_v,timep_ve,timep_vr,timep_ver])

# DRAW VE
X = np.arange(0,nb_items*4,4)   # [0,4,8,..]
drawBar(TIMEP,2,0,'Versioning + Entry')

# DRAW VR
X = np.arange(1,nb_items*4+1,4) # [1,5,9,..]
drawBar(TIMEP,3,1,'Versioning + Return')

# DRAW VER
X = np.arange(2,nb_items*4+2,4) # [2,6,10,..]
drawBar(TIMEP,4,2,'Versioning + Entry + Return')

# DRAW BENCHMARK NAMES
i = 1
for time in TIMEP[:-1]:
    text(i+bar_width/2, -10, os.path.basename(time[0])[:-4], rotation=90, ha='center', va='top')
    i+=4
text(i+bar_width/2, -10,TIMEP[-1][0], rotation=90, ha='center', va='top') # arithmetic mean time (last one)

legend(bbox_to_anchor=(0., 0., 1., -0.55), prop={'size':19}, ncol=3, mode="expand", borderaxespad=0.)

## SAVE/SHOW GRAPH
pdf = PdfPages(PDF_OUTPUT)
pdf.savefig(fig)
pdf.close()
#plt.show()

print('Saved to ' + PDF_OUTPUT)
print('Done!')
