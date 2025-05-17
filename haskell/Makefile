all : Makefile run

run: Makefile build
	stack run

build: Makefile
	stack build --pedantic

lint: Makefile
	-hlint .

format: Makefile
	find . -name '*.hs' | xargs -n 1 hindent

clean: Makefile
	stack clean

