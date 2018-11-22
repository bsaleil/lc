import os
import sys
import glob
import subprocess

# Parse execs from command line arguments
def parse_execs():
    exs = []
    i=4
    while i<len(sys.argv):
        line = sys.argv[i]
        ex = line.split(';')
        exs.append(ex)
        i+=1
    return exs

# This print function detects if stdout is redirected.
# If stdout is redirected, the msg is printed to stdout and stderr for debug purposes
# If stdout is not redirected, the msg is only printed to stdout
def print_m(arg,**kwargs):
    if sys.stdout.isatty(): # not redirected
        print(arg,**kwargs)
    else:
        print(arg,**kwargs) # redirected
        kwargs['file'] = sys.stderr
        print(arg,**kwargs)

# Check cmd line args
if len(sys.argv) <= 4:
    print("Invalid arguments")
    print("Usage:")
    print('   python lckey_to_csv.py path/to/benchmarks path/to/lc "KeyToExtract" [execs]')
    print('Each exec has the form:')
    print('exec_name;arg1;arg2;...;argn')
    sys.exit(0)

BENCH_PATH = os.path.abspath(sys.argv[1]) + '/' # Benchmarks path (contains .scm files)
LC_PATH    = os.path.abspath(sys.argv[2])       # LC path executable path
LC_KEY     = sys.argv[3]                        # Key to extract
EXECS = parse_execs()                           # List of executions

benchmarks = sorted(glob.glob(BENCH_PATH + '*.scm'))

# Print csv header
print_m('X',end='')
for exec in EXECS:
    print_m(';',end='')
    print_m(exec[0],end='')
print_m('')

# For each benchmark
for benchmark in benchmarks:
    benchname = os.path.basename(benchmark).replace('.scm','')
    print_m(benchname,end='')
    # For each exec
    for exec in EXECS:

        # Run exec with --stats
        cmd = [LC_PATH,"--stats"]
        cmd.append(benchmark)
        cmd = cmd + exec[1:]
        result = subprocess.run(cmd,stdout=subprocess.PIPE)
        lines = result.stdout.decode("utf-8").split('\n')

        # Find key:value line
        found = False
        for line in lines:
            keyval = line.split(':')
            if keyval[0] == LC_KEY:
                assert(not found)
                found = True
                val = float(keyval[1].replace(' ','').replace('\t','').replace('\n',''))
                print_m(';',end='')
                print_m(val,end='')
        assert(found)
    print_m('')
