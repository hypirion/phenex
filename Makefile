CC = buildapp
PROJECT = phenex
SRC = $(wildcard *.lisp) $(PROJECT).asd
FLAGS = --asdf-tree $$HOME/quicklisp/dists/quicklisp/software \
        --load-system $(PROJECT) --entry $(PROJECT):main

.PHONY: clean purge

all: $(PROJECT)

$(PROJECT):
	$(CC) $(FLAGS) --output $(PROJECT)

clean:
	-rm -f *.fasl

purge: clean
	-rm -f $(PROJECT)
