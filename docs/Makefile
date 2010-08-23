default: documentation

SRCDIR = ../src

include ../Makefile.incl

BINDIR = ../bin
OUTDIR = ../out
HPCTMPDIR = $(OUTDIR)/hpc-tmp


#---------------------------------------------------------------------------------------
# Directories

HADDOCKDIR  = api
COVERAGEDIR = coverage

#---------------------------------------------------------------------------------------
# Profiling during coverage analysis (with hpc)

PROFILEFLAGS = # -prof -auto-all
PROFILERTS   = # +RTS -p -hc

#---------------------------------------------------------------------------------------
# Documentation targets 

all: api coverage
documentation: all

api: $(HADDOCKDIR)/index.html
pages: exercises.html
coverage: $(COVERAGEDIR)/hpc_index.html

$(HADDOCKDIR)/index.html: $(HS-SOURCES) $(HADDOCKDIR)/prologue
	$(CD) $(SRCDIR); $(HADDOCK) --html -o ../$(DOCDIR)/$(HADDOCKDIR) --prologue=../$(DOCDIR)/$(HADDOCKDIR)/prologue Main.hs

exercises.html: $(BINDIR)/ideas.cgi
	$(BINDIR)/ideas.cgi --make-pages --docs-dir=. --test-dir=../test

$(HPCTMPDIR)/checks$(EXE): $(HS-SOURCES)
	$(MKDIR) -p $(HPCTMPDIR)
	$(GHC) -fhpc --make $(PROFILEFLAGS) -hpcdir $(HPCTMPDIR) \
	   -i$(SRCDIR) -odir $(HPCTMPDIR) -hidir $(HPCTMPDIR) \
	   -o $@ $(SRCDIR)/Main.hs
	
$(HPCTMPDIR)/checks$(EXE).tix: $(HPCTMPDIR)/checks$(EXE)
	$(RM) -f $(HPCTMPDIR)/checks$(EXE).tix
	$(CD) $(HPCTMPDIR); ./checks$(EXE) --make-pages --docs-dir=../../docs --test-dir=../../test $(PROFILERTS) || ($(RM) -f checks$(EXE).tix; exit 1);

$(COVERAGEDIR)/hpc_index.html: $(HPCTMPDIR)/checks$(EXE).tix
	$(HPC) markup --hpcdir=$(HPCTMPDIR) --destdir=$(COVERAGEDIR) $(HPCTMPDIR)/checks$(EXE)
	
#---------------------------------------------------------------------------------------
# Cleaning up

clean:
	$(RM) -rf $(COVERAGEDIR)/*.html
	$(RM) -f  $(HADDOCKDIR)/*.html $(HADDOCKDIR)/*.gif $(HADDOCKDIR)/*.css $(HADDOCKDIR)/*.js
