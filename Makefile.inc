HRC = hrc
EXE = .exe

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
  THUNK_OPT = -F Plsr:lightweight-thunks
else
  THUNK_OPT =
endif

$(PROG)$(EXE):
	$(HRC) -O 3 -ghcO -i../src -ghcO "-odir ." -ghcO -O2 -verbose $(VERBOSITY) \
	    -keep hcr -keep pil -keep obj -link -lm -synchThunks \
	    $(THUNK_OPT) $(VEC_OPT) $(PASSTHRU) $(HRC_EXTRA_OPTS) \
	    $(PROG).hs

clean:
	@rm -f *.hcr *.hi *.dyn_hi *.dyn_o *.o $(PROG).c.*.c $(PROG).c $(PROG).res Harness.* $(PROG) $(PROG).exe
