import os;
import glob;
import subprocess;

os.chdir("/home/bapt/Bureau/baptiste/lazy-comp/")

N_EXEC = 10

benchmarks = sorted(glob.glob("./benchmarks/*.scm"))

print("Benchmark:Interprocedural enabled:Interprocedural disabled")

for benchmark in benchmarks:

    print(benchmark.replace("./benchmarks/","").replace(".scm",""), end=':')

    # entry points ENABLED
    times = [];
    options = ["./lazy-comp", benchmark, "--time"]
    for i in range(0,N_EXEC):
        output = subprocess.check_output(options).decode("utf-8")
        cycles = int(output.split('\n')[2].split(':')[1]);
        times.append(cycles)

    times.remove(max(times))
    times.remove(min(times))
    ave = sum(times) / float(len(times))
    print(ave, end=':');

    # entry points DISABLED
    times = [];
    options = ["./lazy-comp", benchmark, "--time", "--disable-entry-points"]
    for i in range(0,N_EXEC):
        output = subprocess.check_output(options).decode("utf-8")
        cycles = int(output.split('\n')[2].split(':')[1]);
        times.append(cycles)

    times.remove(max(times))
    times.remove(min(times))
    ave = sum(times) / float(len(times))
    print(ave);
