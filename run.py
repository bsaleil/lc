ex=[];

#ex.append(["1.1","strat1",""])
#ex.append(["1.2","strat1","--max-versions 5"])
#ex.append(["1.3","strat1","--max-versions 5 --cc-max 2000 --cr-max 2000"])

# ex.append(["intra","strat1","--disable-entry-points --disable-return-points"])
# ex.append(["interM5","strat1","--max-versions 5"])
# ex.append(["interM5-cv","strat1","--max-versions 5 --enable-const-vers --cc-max 1000 --cr-max 1000 --enable-cxoverflow-fallback"])
# # ex.append(["interM5-cv-s4-1","strat4","--max-versions 5 --enable-const-vers --cc-max 1000 --cr-max 1000 --enable-cxoverflow-fallback --disable-cxoverflow-closest"])
# # ex.append(["interM5-cv-s4-2","strat4","--max-versions 5 --enable-const-vers --cc-max 1000 --cr-max 1000 --enable-cxoverflow-fallback"])
# ex.append(["AA","strat4","--enable-const-vers --cc-max 1000 --enable-cxoverflow-fallback --cr-max 1000 --max-versions 5"])
# ex.append(["AA","strat4","--enable-const-vers --cc-max 1000 --enable-cxoverflow-fallback --cr-max 1000 --max-versions 5 --const-vers-types sym cha boo pai str vec nul voi"])

ex.append(["0","strat1","--max-versions 5"])
ex.append(["1","strat1","--max-versions 5 --enable-const-vers --const-vers-types sym boo cha nul clo"])
ex.append(["2","strat1","--max-versions 10 --enable-const-vers --const-vers-types sym boo cha nul clo"])
# ex.append(["0","strat1","--max-versions 5 --disable-inlined-call"]);
# ex.append(["1","strat1","--max-versions 5 --more-versions 0"]);
# ex.append(["2","strat1","--max-versions 5 --more-versions 1"]);
# ex.append(["3","strat1","--max-versions 5 --more-versions 2"]);
# ex.append(["4","strat1","--max-versions 5 --more-versions 3"]);
# ex.append(["5","strat1","--max-versions 5 --more-versions 4"]);
# ex.append(["6","strat1","--max-versions 5 --more-versions 5"]);
# ex.append(["7","strat1","--max-versions 3 --more-versions 3"]);
# ex.append(["8","strat1","--max-versions 5 --more-versions 10"]);

#
# #----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#
# ex.append(["2.01","strat3","--enable-const-vers --cc-max  500 --cr-max  500 --strat-cst-variation-limit 5 --enable-cxoverflow-fallback --const-vers-types cha voi nul int boo vec str sym flo pai clo"])
# #ex.append(["2.02","strat3","--enable-const-vers --cc-max 1000 --cr-max 1000 --strat-cst-variation-limit 5 --enable-cxoverflow-fallback --const-vers-types cha voi nul int boo vec str sym flo pai clo"])
# ex.append(["2.03","strat3","--enable-const-vers --cc-max 2000 --cr-max 2000 --strat-cst-variation-limit 5 --enable-cxoverflow-fallback --const-vers-types cha voi nul int boo vec str sym flo pai clo"])
# #ex.append(["2.04","strat3","--enable-const-vers --cc-max 3000 --cr-max 3000 --strat-cst-variation-limit 5 --enable-cxoverflow-fallback --const-vers-types cha voi nul int boo vec str sym flo pai clo"])
# ex.append(["2.05","strat3","--enable-const-vers --cc-max  500 --cr-max  500 --strat-cst-variation-limit 5 --enable-cxoverflow-fallback --const-vers-types cha voi nul boo vec str sym pai clo"])
# #ex.append(["2.06","strat3","--enable-const-vers --cc-max 1000 --cr-max 1000 --strat-cst-variation-limit 5 --enable-cxoverflow-fallback --const-vers-types cha voi nul boo vec str sym pai clo"])
# ex.append(["2.07","strat3","--enable-const-vers --cc-max 2000 --cr-max 2000 --strat-cst-variation-limit 5 --enable-cxoverflow-fallback --const-vers-types cha voi nul boo vec str sym pai clo"])
# #ex.append(["2.08","strat3","--enable-const-vers --cc-max 3000 --cr-max 3000 --strat-cst-variation-limit 5 --enable-cxoverflow-fallback --const-vers-types cha voi nul boo vec str sym pai clo"])
# ex.append(["2.09","strat3","--enable-const-vers --cc-max  500 --cr-max  500 --strat-cst-variation-limit 5 --enable-cxoverflow-fallback --const-vers-types cha voi nul boo vec sym clo"])
# #ex.append(["2.10","strat3","--enable-const-vers --cc-max 1000 --cr-max 1000 --strat-cst-variation-limit 5 --enable-cxoverflow-fallback --const-vers-types cha voi nul boo vec sym clo"])
# ex.append(["2.11","strat3","--enable-const-vers --cc-max 2000 --cr-max 2000 --strat-cst-variation-limit 5 --enable-cxoverflow-fallback --const-vers-types cha voi nul boo vec sym clo"])
# #ex.append(["2.12","strat3","--enable-const-vers --cc-max 3000 --cr-max 3000 --strat-cst-variation-limit 5 --enable-cxoverflow-fallback --const-vers-types cha voi nul boo vec sym clo"])
# ex.append(["2.13","strat3","--enable-const-vers --cc-max  500 --cr-max  500 --strat-cst-variation-limit 5 --enable-cxoverflow-fallback --const-vers-types cha voi nul boo sym"])
# #ex.append(["2.14","strat3","--enable-const-vers --cc-max 1000 --cr-max 1000 --strat-cst-variation-limit 5 --enable-cxoverflow-fallback --const-vers-types cha voi nul boo sym"])
# ex.append(["2.15","strat3","--enable-const-vers --cc-max 2000 --cr-max 2000 --strat-cst-variation-limit 5 --enable-cxoverflow-fallback --const-vers-types cha voi nul boo sym"])
# #ex.append(["2.16","strat3","--enable-const-vers --cc-max 3000 --cr-max 3000 --strat-cst-variation-limit 5 --enable-cxoverflow-fallback --const-vers-types cha voi nul boo sym"])
#
# #----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#
# ex.append(["3.01","strat3","--enable-const-vers --cc-max  500 --cr-max  500 --strat-cst-variation-limit 10 --enable-cxoverflow-fallback --const-vers-types cha voi nul int boo vec str sym flo pai clo"])
# #ex.append(["3.02","strat3","--enable-const-vers --cc-max 1000 --cr-max 1000 --strat-cst-variation-limit 10 --enable-cxoverflow-fallback --const-vers-types cha voi nul int boo vec str sym flo pai clo"])
# ex.append(["3.03","strat3","--enable-const-vers --cc-max 2000 --cr-max 2000 --strat-cst-variation-limit 10 --enable-cxoverflow-fallback --const-vers-types cha voi nul int boo vec str sym flo pai clo"])
# #ex.append(["3.04","strat3","--enable-const-vers --cc-max 3000 --cr-max 3000 --strat-cst-variation-limit 10 --enable-cxoverflow-fallback --const-vers-types cha voi nul int boo vec str sym flo pai clo"])
# ex.append(["3.05","strat3","--enable-const-vers --cc-max  500 --cr-max  500 --strat-cst-variation-limit 10 --enable-cxoverflow-fallback --const-vers-types cha voi nul boo vec str sym pai clo"])
# #ex.append(["3.06","strat3","--enable-const-vers --cc-max 1000 --cr-max 1000 --strat-cst-variation-limit 10 --enable-cxoverflow-fallback --const-vers-types cha voi nul boo vec str sym pai clo"])
# ex.append(["3.07","strat3","--enable-const-vers --cc-max 2000 --cr-max 2000 --strat-cst-variation-limit 10 --enable-cxoverflow-fallback --const-vers-types cha voi nul boo vec str sym pai clo"])
# #ex.append(["3.08","strat3","--enable-const-vers --cc-max 3000 --cr-max 3000 --strat-cst-variation-limit 10 --enable-cxoverflow-fallback --const-vers-types cha voi nul boo vec str sym pai clo"])
# ex.append(["3.09","strat3","--enable-const-vers --cc-max  500 --cr-max  500 --strat-cst-variation-limit 10 --enable-cxoverflow-fallback --const-vers-types cha voi nul boo vec sym clo"])
# #ex.append(["3.10","strat3","--enable-const-vers --cc-max 1000 --cr-max 1000 --strat-cst-variation-limit 10 --enable-cxoverflow-fallback --const-vers-types cha voi nul boo vec sym clo"])
# ex.append(["3.11","strat3","--enable-const-vers --cc-max 2000 --cr-max 2000 --strat-cst-variation-limit 10 --enable-cxoverflow-fallback --const-vers-types cha voi nul boo vec sym clo"])
# #ex.append(["3.12","strat3","--enable-const-vers --cc-max 3000 --cr-max 3000 --strat-cst-variation-limit 10 --enable-cxoverflow-fallback --const-vers-types cha voi nul boo vec sym clo"])
# ex.append(["3.13","strat3","--enable-const-vers --cc-max  500 --cr-max  500 --strat-cst-variation-limit 10 --enable-cxoverflow-fallback --const-vers-types cha voi nul boo sym"])
# #ex.append(["3.14","strat3","--enable-const-vers --cc-max 1000 --cr-max 1000 --strat-cst-variation-limit 10 --enable-cxoverflow-fallback --const-vers-types cha voi nul boo sym"])
# ex.append(["3.15","strat3","--enable-const-vers --cc-max 2000 --cr-max 2000 --strat-cst-variation-limit 10 --enable-cxoverflow-fallback --const-vers-types cha voi nul boo sym"])
# #ex.append(["3.16","strat3","--enable-const-vers --cc-max 3000 --cr-max 3000 --strat-cst-variation-limit 10 --enable-cxoverflow-fallback --const-vers-types cha voi nul boo sym"])
#
# #----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#
# ex.append(["4.01","strat3","--enable-const-vers --cc-max  500 --cr-max  500 --strat-cst-variation-limit 30 --enable-cxoverflow-fallback --const-vers-types cha voi nul int boo vec str sym flo pai clo"])
# #ex.append(["4.02","strat3","--enable-const-vers --cc-max 1000 --cr-max 1000 --strat-cst-variation-limit 30 --enable-cxoverflow-fallback --const-vers-types cha voi nul int boo vec str sym flo pai clo"])
# ex.append(["4.03","strat3","--enable-const-vers --cc-max 2000 --cr-max 2000 --strat-cst-variation-limit 30 --enable-cxoverflow-fallback --const-vers-types cha voi nul int boo vec str sym flo pai clo"])
# #ex.append(["4.04","strat3","--enable-const-vers --cc-max 3000 --cr-max 3000 --strat-cst-variation-limit 30 --enable-cxoverflow-fallback --const-vers-types cha voi nul int boo vec str sym flo pai clo"])
# ex.append(["4.05","strat3","--enable-const-vers --cc-max  500 --cr-max  500 --strat-cst-variation-limit 30 --enable-cxoverflow-fallback --const-vers-types cha voi nul boo vec str sym pai clo"])
# # ex.append(["4.06","strat3","--enable-const-vers --cc-max 1000 --cr-max 1000 --strat-cst-variation-limit 30 --enable-cxoverflow-fallback --const-vers-types cha voi nul boo vec str sym pai clo"])
# ex.append(["4.07","strat3","--enable-const-vers --cc-max 2000 --cr-max 2000 --strat-cst-variation-limit 30 --enable-cxoverflow-fallback --const-vers-types cha voi nul boo vec str sym pai clo"])
# # ex.append(["4.08","strat3","--enable-const-vers --cc-max 3000 --cr-max 3000 --strat-cst-variation-limit 30 --enable-cxoverflow-fallback --const-vers-types cha voi nul boo vec str sym pai clo"])
# ex.append(["4.09","strat3","--enable-const-vers --cc-max  500 --cr-max  500 --strat-cst-variation-limit 30 --enable-cxoverflow-fallback --const-vers-types cha voi nul boo vec sym clo"])
# # ex.append(["4.10","strat3","--enable-const-vers --cc-max 1000 --cr-max 1000 --strat-cst-variation-limit 30 --enable-cxoverflow-fallback --const-vers-types cha voi nul boo vec sym clo"])
# ex.append(["4.11","strat3","--enable-const-vers --cc-max 2000 --cr-max 2000 --strat-cst-variation-limit 30 --enable-cxoverflow-fallback --const-vers-types cha voi nul boo vec sym clo"])
# # ex.append(["4.12","strat3","--enable-const-vers --cc-max 3000 --cr-max 3000 --strat-cst-variation-limit 30 --enable-cxoverflow-fallback --const-vers-types cha voi nul boo vec sym clo"])
# ex.append(["4.13","strat3","--enable-const-vers --cc-max  500 --cr-max  500 --strat-cst-variation-limit 30 --enable-cxoverflow-fallback --const-vers-types cha voi nul boo sym"])
# # ex.append(["4.14","strat3","--enable-const-vers --cc-max 1000 --cr-max 1000 --strat-cst-variation-limit 30 --enable-cxoverflow-fallback --const-vers-types cha voi nul boo sym"])
# ex.append(["4.15","strat3","--enable-const-vers --cc-max 2000 --cr-max 2000 --strat-cst-variation-limit 30 --enable-cxoverflow-fallback --const-vers-types cha voi nul boo sym"])
# # ex.append(["4.16","strat3","--enable-const-vers --cc-max 3000 --cr-max 3000 --strat-cst-variation-limit 30 --enable-cxoverflow-fallback --const-vers-types cha voi nul boo sym"])
#
# #----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#
#
# ex.append(["5.01","strat3","--enable-const-vers --cc-max  500 --cr-max  500 --strat-cst-variation-limit 100 --enable-cxoverflow-fallback --const-vers-types cha voi nul int boo vec str sym flo pai clo"])
# #ex.append(["5.02","strat3","--enable-const-vers --cc-max 1000 --cr-max 1000 --strat-cst-variation-limit 100 --enable-cxoverflow-fallback --const-vers-types cha voi nul int boo vec str sym flo pai clo"])
# ex.append(["5.03","strat3","--enable-const-vers --cc-max 2000 --cr-max 2000 --strat-cst-variation-limit 100 --enable-cxoverflow-fallback --const-vers-types cha voi nul int boo vec str sym flo pai clo"])
# #ex.append(["5.04","strat3","--enable-const-vers --cc-max 3000 --cr-max 3000 --strat-cst-variation-limit 100 --enable-cxoverflow-fallback --const-vers-types cha voi nul int boo vec str sym flo pai clo"])
# ex.append(["5.05","strat3","--enable-const-vers --cc-max  500 --cr-max  500 --strat-cst-variation-limit 100 --enable-cxoverflow-fallback --const-vers-types cha voi nul boo vec str sym pai clo"])
# #ex.append(["5.06","strat3","--enable-const-vers --cc-max 1000 --cr-max 1000 --strat-cst-variation-limit 100 --enable-cxoverflow-fallback --const-vers-types cha voi nul boo vec str sym pai clo"])
# ex.append(["5.07","strat3","--enable-const-vers --cc-max 2000 --cr-max 2000 --strat-cst-variation-limit 100 --enable-cxoverflow-fallback --const-vers-types cha voi nul boo vec str sym pai clo"])
# #ex.append(["5.08","strat3","--enable-const-vers --cc-max 3000 --cr-max 3000 --strat-cst-variation-limit 100 --enable-cxoverflow-fallback --const-vers-types cha voi nul boo vec str sym pai clo"])
# ex.append(["5.09","strat3","--enable-const-vers --cc-max  500 --cr-max  500 --strat-cst-variation-limit 100 --enable-cxoverflow-fallback --const-vers-types cha voi nul boo vec sym clo"])
# # ex.append(["5.10","strat3","--enable-const-vers --cc-max 1000 --cr-max 1000 --strat-cst-variation-limit 100 --enable-cxoverflow-fallback --const-vers-types cha voi nul boo vec sym clo"])
# ex.append(["5.11","strat3","--enable-const-vers --cc-max 2000 --cr-max 2000 --strat-cst-variation-limit 100 --enable-cxoverflow-fallback --const-vers-types cha voi nul boo vec sym clo"])
# # ex.append(["5.12","strat3","--enable-const-vers --cc-max 3000 --cr-max 3000 --strat-cst-variation-limit 100 --enable-cxoverflow-fallback --const-vers-types cha voi nul boo vec sym clo"])
# ex.append(["5.13","strat3","--enable-const-vers --cc-max  500 --cr-max  500 --strat-cst-variation-limit 100 --enable-cxoverflow-fallback --const-vers-types cha voi nul boo sym"])
# # ex.append(["5.14","strat3","--enable-const-vers --cc-max 1000 --cr-max 1000 --strat-cst-variation-limit 100 --enable-cxoverflow-fallback --const-vers-types cha voi nul boo sym"])
# ex.append(["5.15","strat3","--enable-const-vers --cc-max 2000 --cr-max 2000 --strat-cst-variation-limit 100 --enable-cxoverflow-fallback --const-vers-types cha voi nul boo sym"])
# # ex.append(["5.16","strat3","--enable-const-vers --cc-max 3000 --cr-max 3000 --strat-cst-variation-limit 100 --enable-cxoverflow-fallback --const-vers-types cha voi nul boo sym"])
#
# #----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#
# ex.append(["6.01","strat3","--enable-const-vers --cc-max  500 --cr-max  500 --strat-cst-variation-limit 300 --enable-cxoverflow-fallback --const-vers-types cha voi nul int boo vec str sym flo pai clo"])
# #ex.append(["6.02","strat3","--enable-const-vers --cc-max 1000 --cr-max 1000 --strat-cst-variation-limit 300 --enable-cxoverflow-fallback --const-vers-types cha voi nul int boo vec str sym flo pai clo"])
# ex.append(["6.03","strat3","--enable-const-vers --cc-max 2000 --cr-max 2000 --strat-cst-variation-limit 300 --enable-cxoverflow-fallback --const-vers-types cha voi nul int boo vec str sym flo pai clo"])
# #ex.append(["6.04","strat3","--enable-const-vers --cc-max 3000 --cr-max 3000 --strat-cst-variation-limit 300 --enable-cxoverflow-fallback --const-vers-types cha voi nul int boo vec str sym flo pai clo"])
# ex.append(["6.05","strat3","--enable-const-vers --cc-max  500 --cr-max  500 --strat-cst-variation-limit 300 --enable-cxoverflow-fallback --const-vers-types cha voi nul boo vec str sym pai clo"])
# # ex.append(["6.06","strat3","--enable-const-vers --cc-max 1000 --cr-max 1000 --strat-cst-variation-limit 300 --enable-cxoverflow-fallback --const-vers-types cha voi nul boo vec str sym pai clo"])
# ex.append(["6.07","strat3","--enable-const-vers --cc-max 2000 --cr-max 2000 --strat-cst-variation-limit 300 --enable-cxoverflow-fallback --const-vers-types cha voi nul boo vec str sym pai clo"])
# # ex.append(["6.08","strat3","--enable-const-vers --cc-max 3000 --cr-max 3000 --strat-cst-variation-limit 300 --enable-cxoverflow-fallback --const-vers-types cha voi nul boo vec str sym pai clo"])
# ex.append(["6.09","strat3","--enable-const-vers --cc-max  500 --cr-max  500 --strat-cst-variation-limit 300 --enable-cxoverflow-fallback --const-vers-types cha voi nul boo vec sym clo"])
# # ex.append(["6.10","strat3","--enable-const-vers --cc-max 1000 --cr-max 1000 --strat-cst-variation-limit 300 --enable-cxoverflow-fallback --const-vers-types cha voi nul boo vec sym clo"])
# ex.append(["6.11","strat3","--enable-const-vers --cc-max 2000 --cr-max 2000 --strat-cst-variation-limit 300 --enable-cxoverflow-fallback --const-vers-types cha voi nul boo vec sym clo"])
# # ex.append(["6.12","strat3","--enable-const-vers --cc-max 3000 --cr-max 3000 --strat-cst-variation-limit 300 --enable-cxoverflow-fallback --const-vers-types cha voi nul boo vec sym clo"])
# ex.append(["6.13","strat3","--enable-const-vers --cc-max  500 --cr-max  500 --strat-cst-variation-limit 300 --enable-cxoverflow-fallback --const-vers-types cha voi nul boo sym"])
# # ex.append(["6.14","strat3","--enable-const-vers --cc-max 1000 --cr-max 1000 --strat-cst-variation-limit 300 --enable-cxoverflow-fallback --const-vers-types cha voi nul boo sym"])
# ex.append(["6.15","strat3","--enable-const-vers --cc-max 2000 --cr-max 2000 --strat-cst-variation-limit 300 --enable-cxoverflow-fallback --const-vers-types cha voi nul boo sym"])
# # ex.append(["6.16","strat3","--enable-const-vers --cc-max 3000 --cr-max 3000 --strat-cst-variation-limit 300 --enable-cxoverflow-fallback --const-vers-types cha voi nul boo sym"])

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


##
##
##
import glob
import subprocess
import sys

files = sorted(glob.glob("/home/bapt/Bureau/these/lc/tools/benchtimes/resultbak/m5inter/" + '/*.scm'))

print('name',end='')
for exe in ex:
    print(':'+exe[0]+'exe:'+exe[0],end='tot')
print('')

for file in files:
    print(file.split("/")[-1].replace(".scm",""),end="")
    sys.stdout.flush()
    for exec in ex:
        # make
        mcmd = ["make","LC_STRAT=" + exec[1]]
        pipe = subprocess.PIPE
        p = subprocess.Popen(mcmd, universal_newlines=True, stdin=pipe, stdout=pipe, stderr=pipe)
        sout, serr = p.communicate()
        rc = p.returncode
        # exec
        if rc == 0:
            ecmd = ["./lc",file,"--time"] + exec[2].split()
            p = subprocess.Popen(ecmd, universal_newlines=True, stdin=pipe, stdout=pipe, stderr=pipe)
            sout, serr = p.communicate()
            rc = p.returncode
            if rc == 0:
                lines = sout.split("\n")
                time_exe = float(lines[9].split(":")[1]) - float(lines[12].split(":")[1])
                time_tot = float(lines[1].split(":")[1]) - float(lines[4].split(":")[1])
                i = 0;
                print(":" + str(time_exe) + ":" + str(time_tot), end='')
            else:
                print(":NUL:NUL",end='')
        else:
            print(":NUL:NUL",end='')
        sys.stdout.flush()
    print("");
