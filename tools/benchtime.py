#!/usr/bin/python3

import sys
import os
import glob
import subprocess
from pylab import *
from matplotlib.backends.backend_pdf import PdfPages
from matplotlib import rc
from matplotlib.ticker import FuncFormatter

plt.rc('text', usetex=True)
plt.rc('font', family='serif')

# -------------------------
# Constants
# -------------------------

SCRIPT_PATH = os.path.dirname(os.path.realpath(__file__)) + '/' # Current script path
LC_PATH     = SCRIPT_PATH + '../'                               # Compiler path
LC_EXEC     = 'lazy-comp'                                       # Compiler exec name
PDF_OUTPUT  = SCRIPT_PATH + 'times.pdf'                         # PDF output file
BENCH_PATH  = LC_PATH + 'benchmarks/*.scm'                      # Benchmarks path
BAR_COLORS  = ["#000000", "#444444", "#888888", "#CCCCCC"]      # Bar colors
ITERS       = 10                                                # Number of iterations >=4 (we remove first, min and max)
FONT_SIZE   = 9                                                 # Font size used to generate pdf using latex (must match the paper font size)

# -------------------------
# Config
# -------------------------

# Change to lc directory
os.chdir(LC_PATH)

# Get all benchmarks full path sorted by name
files = sorted(glob.glob(BENCH_PATH))

# Used as matplotlib formatter
def to_percent(y, position):
    s = str(int(y))
    # The percent symbol needs escaping in latex
    if matplotlib.rcParams['text.usetex'] is True:
        return s + r'$\%$'
    else:
        return s + '%'

# -------------------------
# Exec benchmarks
# and get times
# -------------------------

# Execute ITERS times the file with given options
# Remove first, min and max times and return the sum
def getTime(file,options):
    opts = [LC_PATH + LC_EXEC, file, '--time --verbose-gc']
    opts.extend(options)
    times = []
    for i in range(0,ITERS):
        output = subprocess.check_output(opts).decode("utf-8")
        if "GC" in output: # If gc is triggered, stop the script
            raise Exception('GC is used with benchmark ' + file)
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

    # Exec without versioning
    print('\t* No versioning...')
    time_nv = getTime(file,['--disable-entry-points', '--disable-return-points','--max-versions 0']);

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

    TIME.append([file,time_nv,time_v,time_ve,time_vr,time_ver])

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
matplotlib.rcParams.update({'font.size': FONT_SIZE}) # Set font size of all elements of the graph
#fig = plt.figure('',figsize=(22,9))           # Create new figure
fig = plt.figure('',figsize=(8,3.4))
plt.title('Execution time')                   # Set title
gca().get_xaxis().set_visible(False)          # Hide x values

ylim(0,120)                                   # Set y scale from 0 to 120
xlim(0, nb_items*5)                           # Set x scale from 0 to nb_items*5
fig.subplots_adjust(bottom=0.4)               # Give more space at bottom for benchmark names
plot([0,nb_items*5],[100,100], color="#CCCCCC", linewidth=1) # Draw y=100 line

# Convert times to % times
TIMEP = []
for t in TIME:
    file = t[0]
    time_nv = t[1]
    time_v = t[2]
    time_ve = t[3]
    time_vr = t[4]
    time_ver = t[5]
    # Compute % values relative to time with versioning only
    timep_nv  = 100
    timep_v   = (100*time_v)   / time_nv
    timep_ve  = (100*time_ve)  / time_nv
    timep_vr  = (100*time_vr)  / time_nv
    timep_ver = (100*time_ver) / time_nv

    TIMEP.append([file,timep_nv,timep_v,timep_ve,timep_vr,timep_ver])

# Sort by timep_ver
TIMEP.sort(key=lambda x: x[5])

# Add arithmetic mean values
name = 'arith. mean'
time_nv   = 100
timep_v   = sum(list(map(lambda x: x[2], TIMEP)))/len(files)
timep_ve  = sum(list(map(lambda x: x[3], TIMEP)))/len(files)
timep_vr  = sum(list(map(lambda x: x[4], TIMEP)))/len(files)
timep_ver = sum(list(map(lambda x: x[5], TIMEP)))/len(files)
TIMEP.append([name,time_nv,timep_v,timep_ve,timep_vr,timep_ver])

# DRAW V
X = np.arange(0,nb_items*5,5) # [0,5,10,..]
drawBar(TIMEP,2,0,'Vers.')

# DRAW VE
X = np.arange(1,nb_items*5,5)   # [1,6,11,..]
drawBar(TIMEP,3,1,'Vers. + Entry')

# DRAW VR
X = np.arange(2,nb_items*5,5) # [2,7,12,..]
drawBar(TIMEP,4,2,'Vers. + Return')

# DRAW VER
X = np.arange(3,nb_items*5,5) # [3,6,10,..]
drawBar(TIMEP,5,3,'Vers. + Entry + Return')

# DRAW BENCHMARK NAMES
i = 0
for time in TIMEP[:-1]:
    text(i+2-((1-bar_width)/2), -10, os.path.basename(time[0])[:-4], rotation=90, ha='center', va='top')
    i+=5
text(i+2-((1-bar_width)/2), -10,TIMEP[-1][0], rotation=90, ha='center', va='top') # arithmetic mean time (last one)

legend(bbox_to_anchor=(0., 0., 1., -0.55), prop={'size':FONT_SIZE}, ncol=4, mode="expand", borderaxespad=0.)

# Add '%' symbol to ylabels
formatter = FuncFormatter(to_percent)
plt.gca().yaxis.set_major_formatter(formatter)

## SAVE/SHOW GRAPH
pdf = PdfPages(PDF_OUTPUT)
pdf.savefig(fig)
pdf.close()

print('Saved to ' + PDF_OUTPUT)
print('Done!')
