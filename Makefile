build:
	stack build --haddock --ghc-options '${ghc-options}'

test: build
	rm -f sleep-test.tix
	stack test --ghc-options '${ghc-options}'

lint:
	hlint src

imports:
	./scripts/imports_graph.sh | dot -Tpng -o imports_graph.png

report: test
	mkdir -p test/report
	cd test/report && \
	hpc report ../../sleep-test.tix --srcdir=../.. --exclude=Main && \
	hpc markup ../../sleep-test.tix --srcdir=../.. --exclude=Main

clean:
	rm -f sleep-test.tix
	rm -f imports.png
	rm -Rf test/report
	stack clean

all: lint report

.PHONY: build test lint imports report clean all
