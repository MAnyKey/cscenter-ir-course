all: Metrics.hs learning-to-rank.cabal
	cabal sandbox init &> /dev/null
	cabal install --only-dependencies &> /dev/null
	cabal build
