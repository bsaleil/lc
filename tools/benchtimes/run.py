#!/usr/bin/env python3

import os
import shutil
import glob
import subprocess
import sys
import re

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

    def __init__(self,name,prefixsuffix,ccmd,eext,ecmd,regexms,regexgc):
        self.name = name
        self.prefixsuffix = prefixsuffix
        self.ccmd = ccmd # Compilation cmd
        self.eext = eext # Execution extension
        self.ecmd = ecmd # Execution cmd
        self.regexms = regexms # regex to extract ms time
        self.regexgc = regexgc # regex to extract gc ms time
        self.times = {}

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
                # TODO
                filename = os.path.splitext(os.path.basename(infile))[0]
                print('   ' + filename + '...')
                cmd = self.ccmd.format(infile)
                # compile file
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
            print('   ' + filename + '...')
            def f (x): return x.format(file)
            cmd = list(map(f,self.ecmd))
            pipe = subprocess.PIPE
            p = subprocess.Popen(cmd, universal_newlines=True, stdin=pipe, stdout=pipe, stderr=pipe)
            sout, serr = p.communicate()
            rc = p.returncode

            if (rc != 0) or (serr != '') or ("***" in sout):
                self.execError(file)

            res = re.findall(self.regexms, sout)
            assert (len(res) == 1)
            timems = res[0]

            # Remove gc time
            res = re.findall(self.regexgc, sout)
            assert (len(res) == 0 or len(res) == 1)
            if (len(res) == 1):
                timems = (float(timems) - float(res[0]))
            else:
                timems = float(timems)

            self.times[file] = timems

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

#
# systems = []
# l1 = System("LC-old","",".scm",["lazy-comp-old","{0}","--time"],"(\d+.\d+) ms real time\\n\(")
# l2 = System("LC-new","",".scm",["lazy-comp-new","{0}","--time"],"(\d+.\d+) ms real time\\n\(")
# systems.append(l1)
# systems.append(l2)


def lc_with_options(name,options):
    opts = ["lazy-comp","{0}","--time"]
    opts = opts + options
    return System(name,"LC","",".scm",opts,"(\d+.\d+) ms real time\\n\(","accounting for (\d+) ms real time");
#
systems = []

systems.append(lc_with_options("LC5",      ["--max-versions 5"]))
systems.append(lc_with_options("LC5-bs",   ["--max-versions 5","--enable-const-vers","--const-vers-types boo sym","--enable-cxoverflow-fallback"]))
systems.append(lc_with_options("LC5-bsv",   ["--max-versions 5","--enable-const-vers","--const-vers-types boo sym voi","--enable-cxoverflow-fallback"]))
systems.append(lc_with_options("LC5-bsvc",   ["--max-versions 5","--enable-const-vers","--const-vers-types boo sym voi cha","--enable-cxoverflow-fallback"]))
systems.append(lc_with_options("LC5-bsvcv",   ["--max-versions 5","--enable-const-vers","--const-vers-types boo sym voi cha vec","--enable-cxoverflow-fallback"]))
systems.append(lc_with_options("LC5-bsvcvs",   ["--max-versions 5","--enable-const-vers","--const-vers-types boo sym voi cha vec str","--enable-cxoverflow-fallback"]))

systems.append(lc_with_options("LC10",      ["--max-versions 10"]))
systems.append(lc_with_options("LC10-bs",   ["--max-versions 10","--enable-const-vers","--const-vers-types boo sym","--enable-cxoverflow-fallback"]))
systems.append(lc_with_options("LC10-bsv",   ["--max-versions 10","--enable-const-vers","--const-vers-types boo sym voi","--enable-cxoverflow-fallback"]))
systems.append(lc_with_options("LC10-bsvc",   ["--max-versions 10","--enable-const-vers","--const-vers-types boo sym voi cha","--enable-cxoverflow-fallback"]))
systems.append(lc_with_options("LC10-bsvcv",   ["--max-versions 10","--enable-const-vers","--const-vers-types boo sym voi cha vec","--enable-cxoverflow-fallback"]))
systems.append(lc_with_options("LC10-bsvcvs",   ["--max-versions 10","--enable-const-vers","--const-vers-types boo sym voi cha vec str","--enable-cxoverflow-fallback"]))
# systems.append(lc_with_options("LC15",    ["--max-versions 15"]))
# systems.append(lc_with_options("LC15-boo",["--max-versions 15","--enable-const-vers","--const-vers-types boo","--enable-cxoverflow-fallback"]))
# systems.append(lc_with_options("LC15-cha",["--max-versions 15","--enable-const-vers","--const-vers-types cha","--enable-cxoverflow-fallback"]))
# systems.append(lc_with_options("LC15-clo",["--max-versions 15","--enable-const-vers","--const-vers-types clo","--enable-cxoverflow-fallback"]))
# systems.append(lc_with_options("LC15-str",["--max-versions 15","--enable-const-vers","--const-vers-types str","--enable-cxoverflow-fallback"]))
# systems.append(lc_with_options("LC15-vec",["--max-versions 15","--enable-const-vers","--const-vers-types vec","--enable-cxoverflow-fallback"]))
# systems.append(lc_with_options("LC15-nul",["--max-versions 15","--enable-const-vers","--const-vers-types nul","--enable-cxoverflow-fallback"]))
# systems.append(lc_with_options("LC15-pai",["--max-versions 15","--enable-const-vers","--const-vers-types pai","--enable-cxoverflow-fallback"]))
# systems.append(lc_with_options("LC15-int",["--max-versions 15","--enable-const-vers","--const-vers-types int","--enable-cxoverflow-fallback"]))
# systems.append(lc_with_options("LC15-flo",["--max-versions 15","--enable-const-vers","--const-vers-types flo","--enable-cxoverflow-fallback"]))
# systems.append(lc_with_options("LC15-voi",["--max-versions 15","--enable-const-vers","--const-vers-types voi","--enable-cxoverflow-fallback"]))
# systems.append(lc_with_options("LC15-sym",["--max-versions 15","--enable-const-vers","--const-vers-types sym","--enable-cxoverflow-fallback"]))

# # LC
# l1 = System("LC",
#             "LC", # prefix/suffix
#             "",
#             ".scm",
#             ["lazy-comp","{0}","--time","--max-versions 5"],
#             "(\d+.\d+) ms real time\\n\(",
#             "accounting for (\d+) ms real time")

#
# # Gambit safe
# g2 = System("GambitS",
#             "gsc -:m8000 -exe -o {0}.o1 {0}",
#             ".o1",
#             ["{0}"],
#             "(\d+) ms real time\\n",
#             "accounting for (\d+) ms real time")
#

config = Config()
scriptPath = os.path.dirname(os.path.realpath(__file__))

config.benchPath = scriptPath + '/bench/'
config.resPath = scriptPath + '/result/'
config.benchmarks = sorted(glob.glob(config.benchPath + '*.scm'))

config.prefixPath = scriptPath + '/prefix'
config.suffixPath = scriptPath + '/suffix'

with open(scriptPath + '/num-iters.scm', 'r') as itersfile:
    config.numitersContent = itersfile.read()

#---------------------------------------------------------------------------

runner = Runner(config,systems)

if userWants('Execute only?'):
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
        print(system.times[key],end=';')
    print("");
