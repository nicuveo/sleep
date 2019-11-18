build:
	stack build --ghc-options '${ghc-options}'

test:
	rm -f *.tix
	stack test  --ghc-options '${ghc-options}' --fast

watch:
	rm -f *.tix
	stack test  --ghc-options '${ghc-options}' --fast --file-watch

lint:
	hlint src apps --no-exit-code

doc:
	stack haddock
	stack hoogle -- generate --local

hoogle:
	stack hoogle -- server --local --port=2727 &> /dev/null&

imports:
	./scripts/imports_graph.sh | dot -Tpng -o imports_graph.png

report: test
	mkdir -p tests/report
	cd tests/report && \
	hpc report ../../tumblr-tests.tix --srcdir=../.. --exclude=Main --exclude=Web.Sleep.Tutorial.Tumblr && \
	hpc markup ../../tumblr-tests.tix --srcdir=../.. --exclude=Main --exclude=Web.Sleep.Tutorial.Tumblr

clean:
	rm -f *.tix
	rm -f imports.png
	rm -Rf tests/report
	stack clean

all: lint report

.PHONY: build test watch lint doc hoogle imports report clean all
