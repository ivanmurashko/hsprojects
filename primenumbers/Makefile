SOURCES=src/PrimeNumbers.hs
TESTSOURCES=$(SOURCES) tests/primenumbers.hs
SANDBOX=cabal.sandbox.config

all: dist

$(SANDBOX): primenumbers.cabal
	cabal update
	cabal sandbox init
	cabal install --only-dependencies --enable-tests  primenumbers.cabal

dist: $(SANDBOX) $(SOURCES) primenumbers.cabal
	cabal configure
	cabal build

tests:  $(TESTSOURCES)
	cabal configure --enable-tests
	cabal test

clean:
	cabal sandbox delete
	rm -rf dist
	find . -name "*~" -exec rm -rf {} \;

repl:
	cabal repl

.PHONY: clean repl
