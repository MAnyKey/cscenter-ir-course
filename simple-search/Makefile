
all: submodules libhunspell-1.3 cabal

cabal: *.cabal *.hs
	cabal sandbox init
	cabal install --only-dependencies -j16 --extra-include-dirs=/usr/local/opt/icu4c/include --extra-lib-dirs=/usr/local/opt/icu4c/lib
	cabal configure
	cabal build
	ln -fs dist/build/searcher/searcher searcher
	ln -fs dist/build/indexer/indexer indexer

submodules:
	cd `git rev-parse --show-toplevel`; git submodule update --init

.PHONY:


libhunspell-1.3: .PHONY
	cd hunspell; make || (./configure && make)
	cp hunspell/src/hunspell/.libs/libhunspell-1.3.a ./
