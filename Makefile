benchmarks = 1d-convolution 2d-convolution 7pt-stencil backprojection blackscholes libor nbody treesearch vr
benchmarksbuild = $(addsuffix -build, $(benchmarks))
benchmarksclean = $(addsuffix -clean, $(benchmarks))

.PHONY: all clean 

all: $(benchmarksbuild)

clean: $(benchmarksclean)

%-clean: %
	$(MAKE) -C $< clean

%-build: %
	$(MAKE) -C $<

