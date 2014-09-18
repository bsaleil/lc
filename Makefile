all: build test

build:
	gsc -o main.o1 main.scm
	@echo "#!/bin/bash" > lazy-comp
	@echo -n "gsc -i main.o1 < $$" >> lazy-comp
	@echo "1" >> lazy-comp
	chmod a+x lazy-comp

dev: build

test:
	./run-ut.scm

clean:
	rm -rf *~ *.o* lazy-comp