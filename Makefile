all: main.o1 ast.o1 core.o1 expand.o1
	@echo "#!/bin/bash" > lazy-comp
	@echo -n "gsc -i  core.o1 ast.o1 expand.o1 main.o1 $$" >> lazy-comp
	@echo "@" >> lazy-comp
	chmod u+x lazy-comp

main.o1: main.scm
	gsc -o main.o1 main.scm

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