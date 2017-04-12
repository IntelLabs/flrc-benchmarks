HRC = hrc
EXE = .exe
GHC_OPTS = -O2

$(PROG)$(EXE):
	$(HRC) -ghcO $(GHC_OPTS) -verbose 0 $(PROG).hs
