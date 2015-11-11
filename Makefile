all: float.o1 utils.o1 main.o1 native.o1 mem.o1 ast.o1 core.o1 expand.o1 lib
	cp lazy-comp.template lazy-comp
	chmod u+x lazy-comp

.PHONY: lib
lib:
	gsi ./build-lib

float.o1: float.scm
	gsc -o float.o1 float.scm

utils.o1: utils.scm
	gsc -o utils.o1 utils.scm

main.o1: main.scm
	gsc -o main.o1 main.scm

native.o1: native.scm
	gsc -o native.o1 native.scm

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
