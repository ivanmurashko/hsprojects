SOURCES=src/Experimental.hs
TESTSOURCES=$(SOURCES) tests/experimental.hs
SANDBOX=cabal.sandbox.config

all: dist

$(SANDBOX): experimental.cabal
	cabal update
	cabal sandbox init
	cabal install --only-dependencies --enable-tests  experimental.cabal

dist: $(SANDBOX) $(SOURCES) experimental.cabal
	cabal configure
	cabal build

tests:  $(TESTSOURCES)
	cabal configure --enable-tests
	cabal test

clean:
	cabal sandbox delete
	rm -rf dist
	find . -name "*~" -exec rm -rf {} \;

repl: all
	cabal repl

.PHONY: clean repl
