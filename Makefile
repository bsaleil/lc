GSC_FLAGS=

all: float.o1 utils.o1 main.o1 native.o1 mem.o1 codegen.o1 ast.o1 core.o1 expand.o1 ctx.o1 lib
	cp lazy-comp.template lazy-comp
	chmod u+x lazy-comp

debug: GSC_FLAGS += -debug
debug: all

.PHONY: lib
lib:
	gsi ./build-lib

ctx.o1: ctx.scm
	gsc $(GSC_FLAGS) -o $@ $<

codegen.o1: codegen.scm
	gsc $(GSC_FLAGS) -o $@ $<

float.o1: float.scm
	gsc $(GSC_FLAGS) -o $@ $<

utils.o1: utils.scm
	gsc $(GSC_FLAGS) -o $@ $<

main.o1: main.scm
	gsc $(GSC_FLAGS) -o $@ $<

native.o1: native.scm
	gsc $(GSC_FLAGS) -o $@ $<

mem.o1: mem.scm
	gsc $(GSC_FLAGS) -o $@ $<

ast.o1: ast.scm
	gsc $(GSC_FLAGS) -o $@ $<

core.o1: core.scm
	gsc $(GSC_FLAGS) -o $@ $<

expand.o1: expand.scm
	gsc $(GSC_FLAGS) -o $@ $<

# Run unit tests
test:
	rm ./unit-tests/mutable-out
	./run-ut.scm -lc

# Run full unit tests with and without entry and return points
full-test:
	rm ./unit-tests/mutable-out
	./run-ut.scm -lc -lc-nep -lc-nrp -lc-nep-nrp -lc-n-regalloc

# Clean
clean:
	rm -rf *~ *.o1* lazy-comp
