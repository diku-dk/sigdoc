
MLCOMP ?= mlkit

all: sigdoc

sigdoc: sigdoc.mlb sigdoc.sml Makefile
	$(MLCOMP) -output sigdoc sigdoc.mlb

clean:
	rm -rf *~ MLB run sigdoc *.html
