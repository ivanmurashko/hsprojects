SOURCES=src/PrimeNumbers.hs
TESTSOURCES=$(SOURCES) tests/primenumbers.hs
SANDBOX=.cabal-sandbox

$(SANDBOX): setup

dist: $(SANDBOX) $(SOURCES) primenumbers.cabal
	cabal configure
	cabal build

all: dist

tests:  $(TESTSOURCES)
	cabal configure --enable-tests
	cabal test

setup: primenumbers.cabal
	cabal update
	cabal sandbox init
	cabal install --only-dependencies --enable-tests  primenumbers.cabal

clean:
	rm -rf dist
	rm -rf .cabal-sandbox cabal.sandbox.config
	find . -name "*~" -exec rm -rf {} \;
