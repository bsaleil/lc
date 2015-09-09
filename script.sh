#! /bin/bash

for lang in ./unit-tests/benchmarks/*.scm
do
	echo "$lang "
	./lazy-comp $lang --stats | grep tests
done
