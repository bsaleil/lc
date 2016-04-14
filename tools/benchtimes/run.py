import os
import shutil
import glob
import subprocess

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

    def __init__(self,name,ccmd,eext,ecmd,regexms):
        self.name = name
        self.ccmd = ccmd # Compilation cmd
        self.eext = eext # Execution extension
        self.ecmd = ecmd # Execution cmd
        self.regexms = regexms # regex to extract ms time
        self.times = {}

    def printState(self,str):
        print(str + ' ' + self.name + '...')

    def compileError(self,benchmark):
        msg = 'Error when compiling ' + benchmark + ' with system ' + self.name
        raise Exception(msg)

    def execError(self,benchmark):
        msg = 'Error when execution ' + benchmark + ' with system ' + self.name
        raise Exception(msg)

    def prep(self,config):

        # Print prep task
        self.printState("Prepare system")

        # Copy benchmarks in system tmp dir
        self.tmpDir = config.resPath + '/' + self.name
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
                dst = self.tmpDir + '/' + basename
                # Read source & write prefix + source
                with open(benchmark, 'r') as srcfile, \
                     open(dst,'w') as dstfile:
                    src = srcfile.read()
                    c = config.numitersContent + '\n' + prefix + '\n' + src + '\n' + suffix;
                    dstfile.write(c)

    def compile(self,config):

        # Print compile task
        self.printState("Compile benchmarks for system")

        files = glob.glob(self.tmpDir + '/*.scm')
        assert (len(files) == len(config.benchmarks))
        if self.ccmd == '':
            # No compiler cmd, add .scm extension
            for infile in files:
                os.rename(infile, infile + '.scm')
        else:
            # Else, compile files
            for infile in files:
                cmd = self.ccmd.format(infile)
                # compile file
                code = subprocess.call(cmd,shell=True)
                if code != 0:
                    compileError(benchmark)
                # remove source
                os.remove(infile)

    def execute(self,config):

        # Print execute task
        self.printState("Execute benchmarks for system")

        files = glob.glob(self.tmpDir + '/*' + self.eext)
        assert (len(files) == len(config.benchmarks))

        for file in files:
            def f (x): return x.format(file)
            cmd = list(map(f,self.ecmd))
            pipe = subprocess.PIPE
            p = subprocess.Popen(cmd, universal_newlines=True, stdin=pipe, stdout=pipe, stderr=pipe)
            sout, serr = p.communicate()
            rc = p.returncode
            print(rc)
            print(serr)
            print(sout)
            if (rc != 0) or (serr != '') or ("***" in sout):
                self.execError(file)

            user = sout.split('\n')
            res = re.findall(self.regexms, sout)
            assert (len(res) == 1)
            timems = res[0]
            self.times[file] = timems

#---------------------------------------------------------------------------

class Runner:

    def __init__(self,config,systems):
        self.config = config
        self.systems = systems

    def prep(self):
        for system in self.systems:
            system.prep(config)

    def compile(self):
        for system in self.systems:
            system.compile(config)

    def execute(self):
        for system in self.systems:
            system.execute(config)

#---------------------------------------------------------------------------

systems = []
systems.append(System("GambitC","gsc -exe -o {0}.o1 {0}",".o1",["{0}"],"(\d+) ms real time\\n"))
systems.append(System("GambitI","",".scm",["gsi","{0}"],"(\d+) ms real time\\n"))
systems.append(System("LC-all","",".scm",["lazy-comp","{0}","--time"],"(\d+) ms real time\\n"))

config = Config()
scriptPath = os.path.dirname(os.path.realpath(__file__))

config.benchPath = scriptPath + '/bench/'
config.resPath = scriptPath + '/result/'
config.benchmarks = glob.glob(config.benchPath + '*.scm')

config.prefixPath = scriptPath + '/prefix'
config.suffixPath = scriptPath + '/suffix'

with open(scriptPath + '/num-iters.scm', 'r') as itersfile:
    config.numitersContent = itersfile.read()

if os.path.exists(config.resPath):
    print('Result dir ' + config.resPath + ' exists.')
    r = input('Delete content ? (y/N) ')
    if r == 'y':
        shutil.rmtree(config.resPath)
        os.makedirs(config.resPath)
    else:
        print("Execution aborted.")
else:
    os.makedirs(config.resPath)

#---------------------------------------------------------------------------

runner = Runner(config,systems)
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
