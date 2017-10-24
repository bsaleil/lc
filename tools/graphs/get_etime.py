
# example:
#python get_time.py 4 /path/to/benchs /path/to/lc "test;--max-versions 5;..." "other;--max-versions 10;..."

import re
import os
import sys
import glob
import subprocess

# EXEC TIME
def get(result):
    m = re.findall('CPU time: ([0-9]*.[0-9]*\n)',result)
    assert len(m) == 2, 'Invalid regexp'
    result = float(m[1])
    return result

# Check cmd line args
if len(sys.argv) < 5:
    print("Invalid arguments")
    print("Usage:")
    print('   python thisscript.py nb_iters bench_path lc_path [execs]')
    print('Each exec has the form:')
    print('exec_name;arg1;arg2;...;argn')
    sys.exit(0)

NITERS     = int(sys.argv[1])
BENCH_PATH = os.path.abspath(sys.argv[2]) + '/' # Benchmarks path (contains .scm files)
LC_PATH    = os.path.abspath(sys.argv[3])       # LC path executable path
BASE_CMD   = ["--time"]
EXECS      = []

for i in range(4,len(sys.argv)):
    els = sys.argv[i].split(';')
    name = els[0]
    args = els[1:] + BASE_CMD
    EXECS.append([name,args])

if NITERS < 3:
    print('ERROR: niters is too small')
    sys.exit(0)

benchmarks = sorted(glob.glob(BENCH_PATH + '*.scm'))

def run_and_get(cmd):
    result = subprocess.run(cmd,stdout=subprocess.PIPE)
    strresult = result.stdout.decode("utf-8")
    return get(strresult)

RESULT = []

# For each benchmark
bench_i=1
for benchmark in benchmarks:

    bench_name = os.path.basename(benchmark).replace('.scm','')
    bench_res = [bench_name]

    # Handle cpt
    bench_istr = str(bench_i) if bench_i >=10 else '0' + str(bench_i)
    print('(' + str(bench_istr) + '/' + str(len(benchmarks)) + ') Running benchmark ' + bench_name + '...')

    # For each exec
    for exec in EXECS:

        print('  exec ' + exec[0], end='')
        sys.stdout.flush()
        ex_args = exec[1]
        ex_cmd = [LC_PATH,benchmark] + exec[1]
        ex_res = []

        for i in range(0,NITERS):
            print('.',end='')
            sys.stdout.flush()
            ex_res += [run_and_get(ex_cmd)]
        print('')

        ex_res.remove(max(ex_res))
        ex_res.remove(min(ex_res))
        mean = sum(ex_res) / float(len(ex_res))

        bench_res += [mean]

    print('')
    bench_i += 1

    # Update global result
    RESULT.append(bench_res)

# Pretty print csv
names = list(map(lambda x:str(x[0]),EXECS))
print('benchmark;'+';'.join(names))
for res in RESULT:
    strs = list(map(lambda x:str(x),res))
    print(';'.join(strs))
