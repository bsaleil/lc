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

    def __init__(self,name,ccmd,eext,ecmd,regexms,regexgc):
        self.name = name
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
        prefixPath = config.prefixPath + '/' + self.name + '.scm'
        suffixPath = config.suffixPath + '/' + self.name + '.scm'
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


# GC
systems = []

g1 = System("GambitNS",
            "gsc -:m8000 -exe -o {0}.o1 {0}",".o1",
            ["{0}"],
            "(\d+) ms real time\\n",
            "accounting for (\d+) ms real time")

g2 = System("GambitS",
            "gsc -:m8000 -exe -o {0}.o1 {0}",
            ".o1",
            ["{0}"],
            "(\d+) ms real time\\n",
            "accounting for (\d+) ms real time")

l1 = System("LC-ep-rp",
            "",
            ".scm",
            ["lazy-comp","{0}","--time"],
            "(\d+.\d+) ms real time\\n\(",
            "accounting for (\d+) ms real time")

# l2 = System("LC-rp",
#             "",
#             ".scm",
#             ["lazy-comp","{0}","--time","--disable-entry-points"],
#             "(\d+.\d+) ms real time\\n\(",
#             "accounting for (\d+) ms real time")
#
# l3 = System("LC-ep",
#             "",
#             ".scm",
#             ["lazy-comp","{0}","--time","--disable-return-points"],
#             "(\d+.\d+) ms real time\\n\(",
#             "accounting for (\d+) ms real time")
#
# l4 = System("LC-nep-nrp",
#             "",
#             ".scm",
#             ["lazy-comp","{0}","--time","--disable-entry-points","--disable-return-points"],
#             "(\d+.\d+) ms real time\\n\(",
#             "accounting for (\d+) ms real time")
#
# l5 = System("LC-ep-rp-nra",
#             "",
#             ".scm",
#             ["lazy-comp","{0}","--time","--disable-regalloc-vers"],
#             "(\d+.\d+) ms real time\\n\(",
#             "accounting for (\d+) ms real time")
#
# l6 = System("LC-nep-nrp-nra",
#             "",
#             ".scm",
#             ["lazy-comp","{0}","--time","--disable-entry-points","--disable-return-points","--disable-regalloc-vers"],
#             "(\d+.\d+) ms real time\\n\(",
#             "accounting for (\d+) ms real time")

systems.append(g1)
systems.append(l1)
systems.append(g2)


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

for system in systems:
    print('SYSTEM ' + system.name)
    keys = sorted(list(system.times.keys()))
    for key in keys:
        filename = os.path.splitext(os.path.basename(key))[0]
        filename = os.path.splitext(filename)[0]
        print("{0} {1}".format(filename,system.times[key]))
