
MLCOMP ?= mlkit
SMLPKG ?= smlpkg

all: sigdoc

sigdoc: sigdoc.mlb sigdoc.sml Makefile lib
	$(MLCOMP) -output sigdoc sigdoc.mlb

install:
	cp -p sigdoc $(DESTDIR)/bin/

clean:
	rm -rf *~ MLB run sigdoc *.html
	$(MAKE) -C test clean
	$(MAKE) -C test/full clean

realclean:
	$(MAKE) clean
	rm -rf lib

lib: sml.pkg
	$(SMLPKG) sync
