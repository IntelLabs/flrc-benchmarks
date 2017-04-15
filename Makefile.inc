HRC = hrc
EXE = .exe

NUM_THREADS := 1
GHC_LLVM_OPTS = -fllvm -optlo-O2 -optlo-std-compile-opts

ifeq (y, $(VEC))
  VEC_OPT = -control MilCompile '[{SH} XTREV CIV FREV XDCUYLV REV XTIJKTVX B FREV YBLVZBLV zsFBIft]S' -Varch ViAVX -F MilVectorize:float-reductions
  EXE = .vec.exe
else
  VEC_OPT =
endif

ifeq (y, $(VERBOSE))
  VERBOSITY = 2
else
  VERBOSITY = 0
endif

ifeq (y, $(LWTHUNKS))
  THUNK_OPT = -F Plsr:lightweight-thunks -F Plsr:gc-indirections
  GC_RTS_OPTS = gc remove_indirections gc num_procs=1
else
  THUNK_OPT = -synchThunks
  GC_RTS_OPTS = gc heap_compact_ratio=3 gc num_procs=1
endif

HRC_RTS_OPTS := @PPiler $(GC_RTS_OPTS) ihrThreads $(NUM_THREADS) maxHeap 2048 --
GHC_RTS_OPTS := +RTS -N$(NUM_THREADS) -RTS

$(PROG)$(EXE):
	$(HRC) -O 3 -ghcO -i../src -ghcO "-odir ." -ghcO -O2 -verbose $(VERBOSITY) \
	    -keep hcr -keep pil -keep obj -link -lm \
	    -F Plsr:gc-write-barriers \
	    $(THUNK_OPT) $(VEC_OPT) $(PASSTHRU) $(HRC_EXTRA_OPTS) \
	    $(PROG).hs

run:
	@rm -f $(PROG).res
	./$(PROG)$(EXE) $(HRC_RTS_OPTS) $(ARGS) && mv $(PROG).res $(PROG).hrc.res


run-ghc:
	@rm -f $(PROG).res
	../.cabal-sandbox/bin/$(PROG) $(GHC_RTS_OPTS) $(ARGS) && mv $(PROG).res $(PROG).ghc.res

check: run run-ghc
	diff $(PROG).ghc.res $(PROG).hrc.res

clean:
	@rm -f *.hcr *.hi *.dyn_hi *.dyn_o *.o $(PROG).c.*.c $(PROG).c $(PROG).res Harness.* $(PROG) $(PROG).exe
