GSC_FLAGS= -prelude "(declare (not safe))"
CONFIG_FILE= config-release.scm
LC_STRAT=strat1

all: config types.o1 analyses.o1 float.o1 utils.o1 main.o1 native.o1 mem.o1 codegen.o1 ast.o1 core.o1 expand.o1 ctx.o1 lib
	cp lazy-comp.template lc
	chmod u+x lc



debug: GSC_FLAGS= -debug
debug: CONFIG_FILE= config-debug.scm
debug: all

dummy:
	@:

.PHONY: lib config dummy
lib:
	gsi ./build-lib

config:
	cp $(CONFIG_FILE) config.scm

types.o1: $(and $(LC_STRAT),dummy)
	gsc $(GSC_FLAGS) -o $@ $(LC_STRAT).scm

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

analyses.o1: analyses.scm
	gsc $(GSC_FLAGS) -o $@ $<

# Run unit tests
test:
	rm ./unit-tests/mutable-out -rf
	./run-ut.scm

# Run full unit tests with and without entry and return points
full-test:
	rm ./unit-tests/mutable-out -rf
	./run-ut.scm
	./run-ut.scm --disable-entry-points
	./run-ut.scm --disable-return-points
	./run-ut.scm --disable-entry-points --disable-return-points
	./run-ut.scm --enable-const-vers --enable-cxoverflow-fallback
	./run-ut.scm --max-versions 5
	./run-ut.scm --max-versions 5 --enable-const-vers --enable-cxoverflow-fallback
	./run-ut.scm --max-versions 1

# Clean
clean:
	rm -rf *~ *.o1* lc
