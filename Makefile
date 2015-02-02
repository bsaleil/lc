all: main.o1 mem.o1 ast.o1 core.o1 expand.o1 lib.scm lib
	@echo "#!/bin/bash" > lazy-comp
	@echo -n "gsc -i core.o1 mem.o1 ast.o1 expand.o1 main.o1 $$" >> lazy-comp
	@echo "@" >> lazy-comp
	chmod u+x lazy-comp

.PHONY: lib
lib:
	gsi ./build-lib

main.o1: main.scm
	gsc -o main.o1 main.scm

mem.o1: mem.scm
	gsc -o mem.o1 mem.scm

ast.o1: ast.scm
	gsc -o ast.o1 ast.scm

core.o1: core.scm
	gsc -o core.o1 core.scm

expand.o1: expand.scm
	gsc -o expand.o1 expand.scm

# Run unit tests
test:
	./run-ut.scm

# Clean
clean:
	rm -rf *~ *.o* lazy-comp