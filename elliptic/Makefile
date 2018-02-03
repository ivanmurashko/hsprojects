all: src/Elliptic.hs
	cabal build


configure: elliptic.cabal
	cabal configure	

tests: test/elliptic.hs
	cabal configure --enable-tests
	cabal test

setup: elliptic.cabal
	cabal update
	cabal sandbox init
	cabal install test-framework-hunit
	cabal install hunit-approx

clean:
	rm -rf dist
	rm -rf .cabal-sandbox cabal.sandbox.config
	find . -name "*~" -exec rm -rf {} \;
