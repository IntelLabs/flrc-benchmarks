
PROG = $(shell basename `pwd`)

ARGS = 32 4000 4000

HRC_EXTRA_OPTS = -ghcO -fno-liberate-case -ghcO -funfolding-use-threshold1000 -ghcO -funfolding-keeness-factor1000

include ../Makefile.inc

.PHONY: all

all: $(PROG)$(EXE)
