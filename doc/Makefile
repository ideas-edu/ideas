default: documentation

SRCDIR = ../src

include ../Makefile.incl

OUTDIR = ../out
HPCTMPDIR = $(OUTDIR)/hpc-tmp


#---------------------------------------------------------------------------------------
# Directories

HADDOCKDIR  = haddock
WIKIDIR     = wikipages
EXERCISEDIR = exercises
COVERAGEDIR = coverage

#---------------------------------------------------------------------------------------
# Profiling during coverage analysis (with hpc)

PROFILEFLAGS = # -prof -auto-all
PROFILERTS   = # +RTS -p -hc

#---------------------------------------------------------------------------------------
# Documentation targets 

documentation: haddock wikipages pdfdocs hpc

haddock: $(HADDOCKDIR)/index.html
pdfdocs: $(EXERCISEDIR) $(PDFTARGETS)
hpc: $(COVERAGEDIR)/hpc_index.html

$(HADDOCKDIR)/index.html: $(HS-SOURCES) $(HADDOCKDIR)/prologue
	$(HADDOCK) -o $(HADDOCKDIR) -s "../%F" -p $(HADDOCKDIR)/prologue --html $(HS-SOURCES)

$(WIKIDIR): $(HS-SOURCES)
	$(MKDIR) -p $(WIKIDIR)
	$(CD) ..; $(RUNHASKELL) -isrc src/OpenMath/StrategyToWiki.hs doc/$(WIKIDIR)
	$(TOUCH) $(WIKIDIR) # To get a timestamp
	
$(EXERCISEDIR): $(HS-SOURCES)
	$(RUNHASKELL) -i$(SRCDIR) $(SRCDIR)/Presentation/ExerciseDoc/ExerciseDoc.hs $(EXERCISEDIR)
	make -C $(EXERCISEDIR) || exit 1
	$(TOUCH) $(EXERCISEDIR) # To get a timestamp

$(HPCTMPDIR)/checks$(EXE): $(HS-SOURCES)
	$(MKDIR) -p $(HPCTMPDIR)
	$(GHC) -fhpc --make $(PROFILEFLAGS) -hpcdir $(HPCTMPDIR) \
	   -i$(SRCDIR) -odir $(HPCTMPDIR) -hidir $(HPCTMPDIR) \
	   -o $@ $(SRCDIR)/Common/Checks.hs
	
$(HPCTMPDIR)/checks$(EXE).tix: $(HPCTMPDIR)/checks$(EXE)
	$(RM) -f $(HPCTMPDIR)/checks$(EXE).tix
	$(CD) $(HPCTMPDIR); ./checks$(EXE) ../../test $(PROFILERTS) || ($(RM) -f checks$(EXE).tix; exit 1);
	
$(COVERAGEDIR)/hpc_index.html: $(HPCTMPDIR)/checks$(EXE).tix
	$(MKDIR) -p $(COVERAGEDIR)
	$(HPC) markup --hpcdir=$(HPCTMPDIR) --destdir=$(COVERAGEDIR) $(HPCTMPDIR)/checks$(EXE)
	
#---------------------------------------------------------------------------------------
# Cleaning up

clean:
	$(RM) -rf coverage
	$(RM) -rf wikipages
	$(RM) -f  haddock/*.html haddock/*.gif haddock/*.css haddock/*.js
	make -C $(EXERCISEDIR) clean