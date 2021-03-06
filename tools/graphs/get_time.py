
# example:
# python get_time.py exec 4 -1 /path/to/benchs /path/to/lc "test;--max-versions 5;..." "other;--max-versions 10;..."

import re
import os
import sys
import glob
import signal
import subprocess

# EXEC TIME
# exec only (no comp, no gc) using 2nd execution
def get_exec(result):

    def get_exec_cpu():
        m = re.findall('CPU time: ' + NUMBER_REGEX + '\n', result)
        assert len(m) == 2, 'Invalid regexp'
        return float(m[1][0])

    def get_gc_cpu():
        muser = re.findall('GC user time: '+ NUMBER_REGEX + '\n',result)
        msys = re.findall('GC sys time: '+ NUMBER_REGEX + '\n',result)
        assert len(muser) == 2 and len(msys) == 2, 'Invalid regexp'
        ruser = float(muser[1][0])
        rsys = float(msys[1][0])
        return ruser + rsys

    return get_exec_cpu() - get_gc_cpu()

# COMP TIME
# compilation time only (no exec, no gc and no gc used for compilation) using 1st execution
def get_comp(result):
    m = re.findall('Compilation time \(user time\):' + NUMBER_REGEX + '\n',result)
    assert len(m) == 1, 'Invalid regexp'
    result = float(m[0][0])
    return result

# TOTAL TIME
# total time including compilation time, gc time for compilation & execution, and execution time
def get_total(result):
    m = re.findall('CPU time: '+ NUMBER_REGEX + '\n',result)
    assert len(m) == 1, 'Invalid regexp'
    result = float(m[0][0])
    return result

# Check cmd line args
if len(sys.argv) < 7:
    print('Invalid arguments')
    print('Usage:')
    print('   python thisscript.py time nb_iters bench_path lc_path [execs]')
    print('Each exec has the form:')
    print('exec_name;arg1;arg2;...;argn')
    print('time is "compil", "exec" or "total"')
    sys.exit(0)

# From: https://docs.python.org/3/library/re.html#simulating-scanf
NUMBER_REGEX = '([-+]?(\d+(\.\d*)?|\.\d+)([eE][-+]?\d+)?)'

TIME       = sys.argv[1]
NITERS     = int(sys.argv[2])
TIMEOUT    = int(sys.argv[3]) # timeout in seconds. 0 for no timeout
BENCH_PATH = os.path.abspath(sys.argv[4]) + '/' # Benchmarks path (contains .scm files)
LC_PATH    = os.path.abspath(sys.argv[5])       # LC path executable path
EXECS      = []
BASE_CMD   = []
GET_FUNC   = False

if TIME == 'exec':
    BASE_CMD = ['--time']
    GET_FUNC = get_exec
elif TIME == 'compil':
    BASE_CMD = ['--ctime']
    GET_FUNC = get_comp
elif TIME == 'total':
    BASE_CMD = []
    GET_FUNC = get_total
else:
    print('ERROR: invalid time value')
    sys.exit(0)

for i in range(6,len(sys.argv)):
    els = sys.argv[i].split(';')
    name = els[0]
    args = els[1:] + BASE_CMD
    EXECS.append([name,args])

if NITERS < 3:
    print('ERROR: niters is too small')
    sys.exit(0)

benchmarks = sorted(glob.glob(BENCH_PATH + '*.scm'))

def run_and_get(cmd):
    if TIMEOUT <= 0:
        with subprocess.Popen(cmd, stdout=subprocess.PIPE, preexec_fn=os.setsid) as process:
            out, errs = process.communicate()
            strresult = out.decode('utf-8')
            return GET_FUNC(strresult)
    else:
        with subprocess.Popen(cmd, stdout=subprocess.PIPE, preexec_fn=os.setsid) as process:
            try:
                out, errs = process.communicate(timeout=TIMEOUT)
                strresult = out.decode('utf-8')
                return GET_FUNC(strresult)
            except subprocess.TimeoutExpired:
                os.killpg(process.pid, signal.SIGINT) # kill the group
                out, errs = process.communicate()
                return -1

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
            r = run_and_get(ex_cmd)
            if r < 0:
                ex_res = False
                break
            else:
                ex_res += [r]
        print('')

        if not ex_res:
            bench_res += ["TIMEOUT"]
        else:
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
