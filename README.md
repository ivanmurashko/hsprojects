# hsprojects
There are several Haskell projects 
- elliptic: The simplest elliptic curve implementation
- primenumbers: The prime numbers generator and operations with prime numbers
- experimental: Experiments with haskell
- mathgames: obsolete - see mathexperiments
- stacktest: Test for stack and quickcheck as utest framework
- codewars: tasks from codewars.com
- mathexperiments: Math puzzles in haskell (based on stack)

Each project has its own Makefile.

- Prepare cabal sandbox: make setup
- Configuartion update (after running tests): make configure
- Simple build: make
- Run tests: make tests
- Run repl: make repl