#!/usr/bin/env python3

import os
import shutil
import glob
import subprocess
import sys
import re

if len(sys.argv) < 2:
    print('Invalid option.')
    print('  run.py nexecs [--exec-only]')
    sys.exit(0)

NEXEC = int(sys.argv[1])
EXEC_ONLY = False

if '--exec-only' in sys.argv:
    EXEC_ONLY = True

LC_PATH = os.path.dirname(os.path.abspath(__file__)) + "/../../"
LC_BIN = LC_PATH + "/lc"

CUSTOM_GSC_PATH = "/home/bapt/Bureau/github-repos/gambitBSALEIL"
CUSTOM_GSC_BIN_PATH = CUSTOM_GSC_PATH + "/build/bin"

#---------------------------------------------------------------------------

class Config:
    def __init__(self):
        self.benchPath = ""
        self.resPath = ""
        self.prefixPath = ""
        self.suffixPath = ""
        self.numitersContent = ""

#---------------------------------------------------------------------------

class System:

    def __init__(self,name,prefixsuffix,ccmd,eext,ecmd,regexms,regexgc,time_to_ms,env):
        self.name = name
        self.prefixsuffix = prefixsuffix
        self.ccmd = ccmd # Compilation cmd
        self.eext = eext # Execution extension
        self.ecmd = ecmd # Execution cmd
        self.regexms = regexms # regex to extract ms time
        self.regexgc = regexgc # regex to extract gc ms time
        self.times = {}
        self.gctimes = {}
        self.time_to_ms = time_to_ms
        self.env = env

        self.green = 2
        self.yellow = 3
        self.blue = 4


    def printState(self,color,str):
        print("\033[1;3{0}m*\033[0m ".format(color),end='')
        print(str + ' ' + self.name + '...')

    def compileError(self,benchmark):
        msg = 'Error when compiling ' + benchmark + ' with system ' + self.name
        raise Exception(msg)

    def execError(self,benchmark):
        msg = 'Error when executing ' + benchmark + ' with system ' + self.name
        raise Exception(msg)

    def prep(self,config):

        # Print prep task
        self.printState(self.blue,"Prepare system")

        # Copy benchmarks in system tmp dir
        os.makedirs(self.tmpDir)
        prefixPath = config.prefixPath + '/' + self.prefixsuffix + '.scm'
        suffixPath = config.suffixPath + '/' + self.prefixsuffix + '.scm'
        # Read prefix
        with open(prefixPath, 'r') as prefixfile, \
             open(suffixPath, 'r') as suffixfile:
            prefix = prefixfile.read()
            suffix = suffixfile.read()
            # For each benchmark,
            for benchmark in config.benchmarks:
                basename = os.path.basename(benchmark)
                if basename == "cat.scm" or basename == "wc.scm":
                    infilepath = scriptPath + '/bench/'
                    prefix = prefix + "\n(define BENCH_IN_FILE_PATH \"{0}\")".format(infilepath)
                dst = self.tmpDir + '/' + basename
                # Read source & write prefix + source
                with open(benchmark, 'r') as srcfile, \
                     open(dst,'w') as dstfile:
                    src = srcfile.read()
                    c = prefix + '\n' + config.numitersContent + '\n' + src + '\n' + suffix;
                    dstfile.write(c)

    def compile(self,config):

        # Print compile task
        self.printState(self.yellow,"Compile benchmarks for system")

        files = glob.glob(self.tmpDir + '/*.scm')
        assert (len(files) == len(config.benchmarks))
        if self.ccmd == '':
            # No compiler cmd, add .scm extension
            for infile in files:
                # TODO
                filename = os.path.splitext(os.path.basename(infile))[0]
                print('   ' + filename + '...')
                os.rename(infile, infile + '.scm')
        else:
            # Else, compile files
            for infile in files:
                #
                filename = os.path.splitext(os.path.basename(infile))[0]
                print('   ' + filename + '...')
                cmd = self.ccmd.format(infile)
                # compile file
                if (self.env):
                    code = subprocess.call(cmd,shell=True,env=self.env)
                else:
                    code = subprocess.call(cmd,shell=True)
                if code != 0:
                    self.compileError(infile)
                # remove source
                os.remove(infile)

    def execute(self,config):

        # Print execute task
        self.printState(self.green,"Execute benchmarks with system")

        files = sorted(glob.glob(self.tmpDir + '/*' + self.eext))
        assert (len(files) == len(config.benchmarks))

        for file in files:
            filename = os.path.splitext(os.path.basename(file))[0]
            print('   ' + filename, end='')
            sys.stdout.flush()

            rawTimes = []
            rawGCTimes = []
            for i in range(0,NEXEC):
                print('.', end='')
                sys.stdout.flush()
                def f (x): return x.format(file)
                cmd = list(map(f,self.ecmd))
                pipe = subprocess.PIPE
                if (self.env):
                    p = subprocess.Popen(cmd, universal_newlines=True, stdin=pipe, stdout=pipe, stderr=pipe, env=self.env)
                else:
                    p = subprocess.Popen(cmd, universal_newlines=True, stdin=pipe, stdout=pipe, stderr=pipe)
                sout, serr = p.communicate()
                rc = p.returncode

                if (rc != 0) or (serr != '') or ("***" in sout):
                    timems = self.time_to_ms(-1)
                    rawTimes.append(timems)
                    print("FAIL 1 --->")
                    print(rc)
                    print(serr)
                    print(sout)
                    print(cmd)
                    raise Exception("bar");
                    continue
                    #self.execError(file)

                res = re.findall(self.regexms, sout)

                if (len(res) != 1):
                    timems = self.time_to_ms(-1)
                    rawTimes.append(timems)
                    print("FAIL 2 --->")
                    print(sout)
                    raise Exception("foo");
                    continue

                timems = res[0]
                gctimems = 0.0;
                # Remove gc time
                if (self.regexgc):
                    res = re.findall(self.regexgc, sout)
                    assert (len(res) == 0 or len(res) == 1)
                    if (len(res) == 1):
                        timems = (float(timems) - float(res[0]))
                        gctimems = float(res[0])
                    else:
                        assert "no collections" in sout, "Invalid regexp"
                        timems = float(timems)
                        gctimems = 0.0
                else:
                    assert "no collections" in sout, "Invalid regexp"
                    timems = float(timems)
                    gctimems = 0.0

                timems = self.time_to_ms(timems)
                gctimems = self.time_to_ms(gctimems)
                rawTimes.append(timems)
                rawGCTimes.append(gctimems)

            print('')
            # Remove min, max and compute mean
            rawTimes.remove(max(rawTimes))
            rawTimes.remove(min(rawTimes))
            rawGCTimes.remove(max(rawGCTimes))
            rawGCTimes.remove(min(rawGCTimes))
            mean = sum(rawTimes) / float(len(rawTimes))
            gcmean = sum(rawGCTimes) / float(len(rawGCTimes))
            self.times[file] = mean
            self.gctimes[file] = gcmean

#---------------------------------------------------------------------------

class Runner:

    def __init__(self,config,systems):
        self.config = config
        self.systems = systems
        for system in systems:
            system.tmpDir = config.resPath + '/' + system.name

    def prep(self):
        for system in self.systems:
            system.prep(self.config)

    def compile(self):
        for system in self.systems:
            system.compile(self.config)

    def execute(self):
        for system in self.systems:
            system.execute(self.config)

#---------------------------------------------------------------------------

def userWants(str):
    r = input(str + ' (y/N) ')
    return r == 'y'

def lc_with_options(name,options):
    opts = [LC_BIN,"{0}","--time"]
    opts = opts + options
    newenv = os.environ.copy()
    newenv["PATH"] = CUSTOM_GSC_BIN_PATH + ":" + newenv["PATH"]
    return System(name,"LC","",".scm",opts,"(?:.*\n){13}Real time: ([^\n]*)\n","(?:.*\n){16}GC real time: ([^\n]*)\n",lambda x: x*1000.0,newenv)

def lc_with_options_notime(name,options):
    opts = [LC_BIN,"{0}"]
    opts = opts + options
    newenv = os.environ.copy()
    newenv["PATH"] = CUSTOM_GSC_BIN_PATH + ":" + newenv["PATH"]
    return System(name,"LC","",".scm",opts,"(?:.*\n){4}Real time: ([^\n]*)\nGC","(?:.*\n){7}GC real time: ([^\n]*)\n",lambda x: x*1000.0,newenv)

def lcf64v_with_options(name,options):
    opts = [LC_BIN,"{0}","--time"]
    opts = opts + options
    newenv = os.environ.copy()
    newenv["PATH"] = CUSTOM_GSC_BIN_PATH + ":" + newenv["PATH"]
    return System(name,"LCf64v","",".scm",opts,"(?:.*\n){13}Real time: ([^\n]*)\n","(?:.*\n){16}GC real time: ([^\n]*)\n",lambda x: x*1000.0,newenv)

def lcf64v_with_options_notime(name,options):
    opts = [LC_BIN,"{0}"]
    opts = opts + options
    newenv = os.environ.copy()
    newenv["PATH"] = CUSTOM_GSC_BIN_PATH + ":" + newenv["PATH"]
    return System(name,"LCf64v","",".scm",opts,"(?:.*\n){4}Real time: ([^\n]*)\nGC","(?:.*\n){7}GC real time: ([^\n]*)\n",lambda x: x*1000.0,newenv)

def pycket_no_options(name):
    opts = ["/home/bapt/Bureau/testPycket/pycket/pycket-c","{0}"]
    newenv = os.environ.copy()
    newenv["PLTHOME"] = "/home/bapt/Bureau/testPycket/racket/"
    newenv["PATH"] = "/home/bapt/Bureau/testPycket/racket/racket/bin/:" + newenv["PATH"]
    newenv["PYTHONPATH"] = "/home/bapt/Bureau/testPycket/pycket:/home/bapt/Bureau/testPycket/pycket/pypy"
    return System(name,"Pycket","",".scm",opts,"real time:[\s]*([\S]*)[\s]*gc time", "gc time:[\s]*([\S]*)[\s]*", lambda x: x, newenv)

def gambit_boxunbox_no_options(name):
    opts = []
    cmd = "/home/bapt/Bureau/gambitBOXUNBOX/gsc/gsc -:=/home/bapt/Bureau/gambitBOXUNBOX/ -exe -o {0}.o1 {0}"
    return System(name,name,cmd,".o1",["{0}"],"(\d+) ms real time\\n","accounting for (\d+) ms real time",lambda x: x,False)

def gambit_no_options(name,gcsize):
    opts = []
    cmd = "gsc -:m"+ str(gcsize) + " -exe -o {0}.o1 {0}"
    return System(name,name,cmd,".o1",["{0}"],"(\d+) ms real time\\n","accounting for (\d+) ms real time",lambda x: x,False)

def chez_no_options(name):
    opts = []
    cmd = "echo '(compile-program \"{0}\" \"{0}.so\")' | /home/bapt/Bureau/chezscheme/chez-build/bin/scheme -q"
    newenv = os.environ.copy()
    newenv["SCHEMEHEAPDIRS"] = "/home/bapt/Bureau/chezscheme/chez-build/lib/csv9.5.2/a6le/"
    return System(name,"ChezScheme",cmd,".so",["/home/bapt/Bureau/chezscheme/chez-build/bin/scheme","--program","{0}"],"\\n([^s]*)s elapsed real time","real time, including ([^s]*)s collecting",lambda x: x*1000.0,newenv)

#
systems = []

# systems.append(lc_with_options("LC", []))
# # LC
# systems.append(lc_with_options("m5intra",  ["--max-versions 5","--disable-entry-points","--disable-return-points"]))
# systems.append(lc_with_options("m5eponly", ["--max-versions 5","--disable-return-points"]))
# systems.append(lc_with_options("m5rponly", ["--max-versions 5","--disable-entry-points"]))

# # tagging !opt
# systems.append(lc_with_options("tag-noopt-6g", ["--disable-float-unboxing","--min-heap 6500000"]))
# # tagging opt
# systems.append(lc_with_options("tag-opt", []))
# # tagging opt (6gb heap)
# systems.append(lc_with_options("tag-opt-6g", ["--min-heap 6500000"]))
# # nan-boxing !opt
# systems.append(lc_with_options("nan-noopt-6g", ["--nan-boxing","--disable-float-unboxing"]))
# # nan-boxing opt
# systems.append(lc_with_options("nan-opt-6g", ["--nan-boxing"]))

# LC
# systems.append(lc_with_options("LC", []))

# LC
# systems.append(lc_with_options("LC", []))
# systems.append(lc_with_options("LC-noopt", ["--disable-float-unboxing"]))
# systems.append(lc_with_options("LC-nan", ["--nan-boxing"]))
# systems.append(lc_with_options("LC-nan-noopt", ["--nan-boxing","--disable-float-unboxing"]))

# systems.append(lcf64v_with_options("LCf64v", ["--disable-pair-tag"]))
# systems.append(lcf64v_with_options("LCf64v-noopt", ["--disable-pair-tag","--disable-float-unboxing"]))
# systems.append(lcf64v_with_options("LCf64v-nan", ["--disable-pair-tag","--nan-boxing"]))
# systems.append(lcf64v_with_options("LCf64v-nan-noopt", ["--disable-pair-tag","--nan-boxing","--disable-float-unboxing"]))

# systems.append(lcf64v_with_options("tag-intra-noopt", ["--disable-pair-tag","--max-versions 5","--disable-float-unboxing","--disable-entry-points","--disable-return-points"]))
# systems.append(lcf64v_with_options("tag-inter-noopt", ["--disable-pair-tag","--max-versions 5","--disable-float-unboxing"]))
#
# systems.append(lcf64v_with_options("nan-intra-noopt", ["--disable-pair-tag","--max-versions 5","--disable-float-unboxing","--disable-entry-points","--disable-return-points","--nan-boxing"]))
# systems.append(lcf64v_with_options("nan-inter-noopt", ["--disable-pair-tag","--max-versions 5","--disable-float-unboxing","--nan-boxing"]))
#
# systems.append(lcf64v_with_options("nan-intra-opt",   ["--disable-pair-tag","--max-versions 5","--disable-entry-points","--disable-return-points","--nan-boxing"]))
# systems.append(lcf64v_with_options("nan-inter-opt",   ["--disable-pair-tag","--max-versions 5","--nan-boxing"]))
#
# systems.append(lcf64v_with_options("tag-intra-opt",   ["--disable-pair-tag","--max-versions 5","--disable-entry-points","--disable-return-points"]))
# systems.append(lcf64v_with_options("tag-inter-opt",   ["--disable-pair-tag","--max-versions 5"]))

# systems.append(chez_no_options("ChezScheme"))
# systems.append(lcf64v_with_options("tag-inter-opt", ["--disable-pair-tag","--max-versions 5"]))

# Gambit
# systems.append(gambit_no_options("Gambit", 512000))
# systems.append(gambit_no_options("Gambitf64v", 512000))

systems.append(chez_no_options("ChezAOT"))
# systems.append(gambit_no_options("Gambit", 512000))
# systems.append(lcf64v_with_options("LCf64v", ["--max-versions 5"]))

# LC/Pycket (exec+compil time)
# systems.append(lc_with_options_notime("LC", []))
# systems.append(lcf64v_with_options_notime("LCf64v", []))
#
# systems.append(gambit_boxunbox_no_options("GambitBU"))
# systems.append(gambit_boxunbox_no_options("GambitBUf64v"))
# systems.append(pycket_no_options("Pycket"));
# systems.append(lc_with_options_notime("LC5", ["--max-versions 5"]));
# systems.append(lcf64v_with_options_notime("LC5f64", ["--max-versions 5"]));
# systems.append(lc_with_options_notime("LCnaive", ["--max-versions 0", "--disable-float-unboxing", "--disable-entry-points", "--disable-return-points", "--disable-regalloc-vers"]))

# # LC/Gambit/LC (exec time)
# systems.append(gambit_no_options("Gambit", 512000))
# systems.append(gambit_no_options("Gambitf64v", 512000))
# systems.append(lc_with_options("LC5", ["--max-versions 5"]));
# systems.append(lcf64v_with_options("LC5f64", ["--max-versions 5"]));
# systems.append(lc_with_options("LCnaive", ["--max-versions 0", "--disable-float-unboxing", "--disable-entry-points", "--disable-return-points", "--disable-regalloc-vers"]));
# systems.append(lcf64v_with_options("LCf64naive", ["--max-versions 0", "--disable-float-unboxing", "--disable-entry-points", "--disable-return-points", "--disable-regalloc-vers"]));

config = Config()
scriptPath = os.path.dirname(os.path.realpath(__file__))

distPath = LC_PATH + "/tools/benchtimes"

config.benchPath = distPath + '/bench/'
config.resPath = scriptPath + '/result/'
config.benchmarks = sorted(glob.glob(config.benchPath + '*.scm'))

config.prefixPath = distPath + '/prefix'
config.suffixPath = distPath + '/suffix'

with open(scriptPath + '/num-iters.scm', 'r') as itersfile:
    config.numitersContent = itersfile.read()

#---------------------------------------------------------------------------

runner = Runner(config,systems)

if EXEC_ONLY or userWants('Execute only?'):
    runner.execute()
else:
    if os.path.exists(config.resPath):
        print('ERROR - Result dir ' + config.resPath + ' exists.')
        sys.exit(0)
    os.makedirs(config.resPath);

    runner.prep()
    runner.compile()
    runner.execute()

# Print header line
print("benchmark;",end='')
for system in systems:
    print(system.name,end=';')
    print(system.name+"(gc)",end=';')
print("")
# Print times
for benchmark in config.benchmarks:
    # Get and print benchmark name
    benchname = os.path.splitext(os.path.basename(benchmark))[0]
    print(benchname,end=';')
    # For each system, print time
    for system in systems:
        key = system.tmpDir + "/" + os.path.basename(benchmark) + system.eext
        assert(key in system.times.keys())
        assert(key in system.gctimes.keys())
        print(system.times[key],end=';')
        print(system.gctimes[key],end=';')
    print("");
