benchmarks = 1d-convolution 2d-convolution 7pt-stencil backprojection blackscholes libor nbody treesearch vr

.PHONY: all $(benchmarks)

all: $(benchmarks)

$(benchmarks):
	$(MAKE) -C $@ 
