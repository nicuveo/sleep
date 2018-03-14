build:
	stack build --ghc-options '${ghc-options}'

test:
	rm -f sleep-test.tix
	stack test  --ghc-options '${ghc-options}'

lint:
	hlint src

clean:
	stack clean

report: test
	mkdir -p test/report
	cd test/report && \
	hpc report ../../sleep-test.tix --srcdir=../.. --exclude=Main && \
	hpc markup ../../sleep-test.tix --srcdir=../.. --exclude=Main

all: lint report

.PHONY: build test lint clean report all
