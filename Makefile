benchmarks = 1d-convolution 2d-convolution 7pt-stencil backprojection blackscholes libor nbody treesearch vr
benchmarksbuild = $(addsuffix -build, $(benchmarks))
benchmarksclean = $(addsuffix -clean, $(benchmarks))
benchmarksrun = $(addsuffix -run, $(benchmarks))
benchmarksrunghc = $(addsuffix -run-ghc, $(benchmarks))
benchmarkscheck = $(addsuffix -check, $(benchmarks))

.PHONY: all clean cleanall build-ghc clean-ghc

build: $(benchmarksbuild)

run: $(benchmarksrun)

run-ghc: $(benchmarksrun-ghc)

check: $(benchmarkscheck)

clean: $(benchmarksclean) clean-ghc

cleanall: clean clean-ghc-sandbox

%-clean: %
	$(MAKE) -C $< clean

%-build: %
	$(MAKE) -C $<

build-ghc: .cabal-sandbox
	cabal install

%-run: %
	$(MAKE) -C $< run

%-run-ghc: % build-ghc
	$(MAKE) -C $< run-ghc

%-check: % build-ghc
	$(MAKE) -C $< check

.cabal-sandbox:
	cabal sandbox init

clean-ghc:
	cabal clean

clean-ghc-sandbox:
	cabal sandbox delete
