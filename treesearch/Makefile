
PROG = $(shell basename `pwd`)

ARGS = block 24 100000000

HRC_EXTRA_OPTS = -ghcO -fno-liberate-case -ghcO -funfolding-use-threshold1000 -ghcO -funfolding-keeness-factor1000 -ghcO -DREPA

include ../Makefile.inc

.PHONY: all

all: $(PROG)$(EXE)
