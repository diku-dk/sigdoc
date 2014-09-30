
MLCOMP ?= mlkit

all: sigdoc

sigdoc: sigdoc.mlb sigdoc.sml Makefile
	$(MLCOMP) -output sigdoc sigdoc.mlb

install:
	cp -p sigdoc $(DESTDIR)/bin/

clean:
	rm -rf *~ MLB run sigdoc *.html
