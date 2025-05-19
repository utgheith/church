.PHONY: all run test build lint format clean

all : Makefile run

run: Makefile
	stack run

test: Makefile
	stack test

build: Makefile
	stack build --pedantic

lint: Makefile
	-hlint .

format: Makefile
	find . -name '*.hs' | xargs -n 1 hindent

clean: Makefile
	stack clean

