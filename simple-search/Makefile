
all: submodules libhunspell-1.3 cabal

cabal: *.cabal *.hs
	cabal sandbox init
	cabal install --only-dependencies -j16
	cabal build

submodules:
	cd `git rev-parse --show-toplevel`; git submodule update --init

.PHONY:

libhunspell-1.3: .PHONY
	cd hunspell; make || (./configure && make)
	cp hunspell/src/hunspell/.libs/libhunspell-1.3.a ./
	cp hunspell/src/hunspell/.libs/libhunspell-1.3.so ./
