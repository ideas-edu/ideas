default: documentation

SRCDIR = ../src

include ../Makefile.incl

BINDIR = ../bin
OUTDIR = ../out
HPCTMPDIR = $(OUTDIR)/hpc-tmp


#---------------------------------------------------------------------------------------
# Directories

HADDOCKDIR  = api
EXERCISEDIR = exercises
COVERAGEDIR = coverage

#---------------------------------------------------------------------------------------
# Profiling during coverage analysis (with hpc)

PROFILEFLAGS = # -prof -auto-all
PROFILERTS   = # +RTS -p -hc

#---------------------------------------------------------------------------------------
# Documentation targets 

all: api pages tests coverage latexrules
documentation: all

api: $(HADDOCKDIR)/index.html
pages: exercises.html
tests: tests.html
latexrules: $(EXERCISEDIR) $(PDFTARGETS)
coverage: $(COVERAGEDIR)/hpc_index.html

$(HADDOCKDIR)/index.html: $(HS-SOURCES) $(HADDOCKDIR)/prologue
	$(CD) $(SRCDIR); $(HADDOCK) --html -o ../$(DOCDIR)/$(HADDOCKDIR) --prologue=../$(DOCDIR)/$(HADDOCKDIR)/prologue Service/Main.hs

exercises.html: $(BINDIR)/ideas.cgi
	$(BINDIR)/ideas.cgi --make-pages=.

$(EXERCISEDIR): $(BINDIR)/ideas.cgi
	$(BINDIR)/ideas.cgi --make-rules=exercises
	make -C $(EXERCISEDIR) all
	$(TOUCH) $(EXERCISEDIR) # To get a timestamp

$(HPCTMPDIR)/checks$(EXE): $(HS-SOURCES)
	$(MKDIR) -p $(HPCTMPDIR)
	$(GHC) -fhpc --make $(PROFILEFLAGS) -hpcdir $(HPCTMPDIR) \
	   -i$(SRCDIR) -odir $(HPCTMPDIR) -hidir $(HPCTMPDIR) \
	   -o $@ $(SRCDIR)/Service/Main.hs
	
$(HPCTMPDIR)/checks$(EXE).tix: $(HPCTMPDIR)/checks$(EXE)
	$(RM) -f $(HPCTMPDIR)/checks$(EXE).tix
	$(CD) $(HPCTMPDIR); ./checks$(EXE) --self-check=../../test $(PROFILERTS) > tests.txt 2> /dev/null || ($(RM) -f checks$(EXE).tix; exit 1);

tests.html: $(HPCTMPDIR)/checks$(EXE).tix
	$(RUNHASKELL) -i$(SRCDIR) -odir$(OUTDIR) -hidir$(OUTDIR) $(SRCDIR)/Documentation/TestsPage.hs $(HPCTMPDIR)/tests.txt $@

$(COVERAGEDIR)/hpc_index.html: $(HPCTMPDIR)/checks$(EXE).tix
	$(HPC) markup --hpcdir=$(HPCTMPDIR) --destdir=$(COVERAGEDIR) $(HPCTMPDIR)/checks$(EXE)
	
#---------------------------------------------------------------------------------------
# Cleaning up

clean:
	$(RM) -rf $(COVERAGEDIR)/*.html
	$(RM) -rf wikipages
	$(RM) -f  $(HADDOCKDIR)/*.html $(HADDOCKDIR)/*.gif $(HADDOCKDIR)/*.css $(HADDOCKDIR)/*.js
	make -C $(EXERCISEDIR) clean
