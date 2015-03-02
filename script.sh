#! /bin/bash

for lang in ./unit-tests/benchmarks/*
do
	echo "$lang" ;
	echo "----------------------------------------" >>out ;
	echo  "$lang" | sed -e 's/.\/unit-tests\/benchmarks\///g' >>out ;
	./lazy-comp $lang | egrep '(RCX|RBX)' | sed -e "s/RBX/TOTAL/g" | sed -e "s/RCX/LIB  /g" >> out ;
done