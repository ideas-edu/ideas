default: documentation

SRCDIR = ../src

include ../Makefile.incl

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

documentation: api coverage exercisedocs

api: $(HADDOCKDIR)/index.html
exercisedocs: $(EXERCISEDIR) $(PDFTARGETS)
coverage: $(COVERAGEDIR)/hpc_index.html

$(HADDOCKDIR)/index.html: $(HS-SOURCES) $(HADDOCKDIR)/prologue
	$(CD) $(SRCDIR); $(HADDOCK) --html -o ../$(DOCDIR)/$(HADDOCKDIR) --prologue=../$(DOCDIR)/$(HADDOCKDIR)/prologue Service/Main.hs
	
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
	$(HPC) markup --hpcdir=$(HPCTMPDIR) --destdir=$(COVERAGEDIR) $(HPCTMPDIR)/checks$(EXE)
	
#---------------------------------------------------------------------------------------
# Cleaning up

clean:
	$(RM) -rf $(COVERAGEDIR)/*.html
	$(RM) -rf wikipages
	$(RM) -f  haddock/*.html haddock/*.gif haddock/*.css haddock/*.js
	make -C $(EXERCISEDIR) clean
