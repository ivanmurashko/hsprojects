all: src/PrimeNumbers.hs
	cabal build

configure: primenumbers.cabal
	cabal configure	

tests: tests/primenumbers.hs src/PrimeNumbers.hs
	cabal configure --enable-tests
	cabal test

setup: primenumbers.cabal
	cabal update
	cabal sandbox init
	cabal install test-framework-hunit
	cabal install hunit-approx

clean:
	rm -rf dist
	rm -rf .cabal-sandbox cabal.sandbox.config
	find . -name "*~" -exec rm -rf {} \;
