SOURCES=src/MathGames.hs
TESTSOURCES=$(SOURCES) tests/mathgames.hs
SANDBOX=cabal.sandbox.config

all: dist

$(SANDBOX): mathgames.cabal
	cabal update
	cabal sandbox init
	cabal install --only-dependencies --enable-tests  mathgames.cabal

dist: $(SANDBOX) $(SOURCES) mathgames.cabal
	cabal configure
	cabal build

tests: $(TESTSOURCES) $(SANDBOX) $(SOURCES) mathgames.cabal
	cabal configure --enable-tests
	cabal test

clean:
	cabal sandbox delete
	rm -rf dist
	find . -name "*~" -exec rm -rf {} \;

repl: all
	cabal repl

.PHONY: clean repl
