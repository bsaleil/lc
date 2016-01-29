all: float.o1 utils.o1 main.o1 native.o1 mem.o1 codegen.o1 ast.o1 core.o1 expand.o1 lib
	cp lazy-comp.template lazy-comp
	chmod u+x lazy-comp

.PHONY: lib
lib:
	gsi ./build-lib

codegen.o1: codegen.scm
	gsc -debug -o codegen.o1 codegen.scm

float.o1: float.scm
	gsc -debug -o float.o1 float.scm

utils.o1: utils.scm
	gsc -debug -o utils.o1 utils.scm

main.o1: main.scm
	gsc -debug -o main.o1 main.scm

native.o1: native.scm
	gsc -debug -o native.o1 native.scm

mem.o1: mem.scm
	gsc -debug -o mem.o1 mem.scm

ast.o1: ast.scm
	gsc -debug -o ast.o1 ast.scm

core.o1: core.scm
	gsc -debug -o core.o1 core.scm

expand.o1: expand.scm
	gsc -debug -o expand.o1 expand.scm

# Run unit tests
test:
	./run-ut.scm -lc

# Run full unit tests with and without entry and return points
full-test:
	./run-ut.scm -lc -lc-nep -lc-nrp -lc-nep-nrp

# Clean
clean:
	rm -rf *~ *.o* lazy-comp
