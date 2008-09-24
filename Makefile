default: service solvergui
all: binaries unit-tests documentation

SRCDIR = src

include Makefile.incl

binaries: service laservice solvergui

service: $(BINDIR)/service.cgi
laservice: $(BINDIR)/laservice.cgi
solvergui: $(BINDIR)/solvergui$(EXE)

$(BINDIR)/service.cgi: $(HS-SOURCES)
	$(MKDIR) -p $(BINDIR) $(OUTDIR)
	$(GHC) $(GHCFLAGS) -o $@ src/Service/Main.hs
	$(STRIP) $@

$(BINDIR)/laservice.cgi: $(HS-SOURCES)
	$(MKDIR) -p $(BINDIR) $(OUTDIR)
	$(GHC) $(GHCFLAGS) -o $@ src/OpenMath/Main.hs
	$(STRIP) $@

$(BINDIR)/solvergui$(EXE): $(HS-SOURCES) $(GLADE-SOURCES)
ifeq ($(GTK), yes)
	$(MKDIR) -p $(BINDIR) $(OUTDIR)
	$(GHC) $(GHCFLAGS) -isrc/Presentation/ExerciseAssistant -o $@ src/Presentation/ExerciseAssistant/ExerciseAssistant.hs
	$(STRIP) $@
	$(CP) src/Presentation/ExerciseAssistant/exerciseassistant.glade bin/
	$(CP) src/Presentation/ExerciseAssistant/ounl.jpg bin/	
endif

#---------------------------------------------------------------------------------------
# Other directories

documentation: $(DOCDIR)

$(DOCDIR): $(HS-SOURCES)	
	make -C $(DOCDIR) || exit 1
	$(TOUCH) $(DOCDIR) # To get a timestamp

unit-tests: $(TESTDIR)

$(TESTDIR): $(HS-SOURCES) $(BINDIR)/service.cgi
	make -C $(TESTDIR) || exit 1
	$(TOUCH) $(TESTDIR) # To get a timestamp
	
#---------------------------------------------------------------------------------------
# Helper targets

ghci:
	$(MKDIR) -p $(OUTDIR)
	$(GHCI) -i$(SRCDIR) -i$(SRCDIR)/Presentation/ExerciseAssistant -odir $(OUTDIR) -hidir $(OUTDIR) $(GHCWARN)
	
run: solvergui
	$(BINDIR)/solvergui$(EXE)
	
nolicense:
	find src -name *.hs -print0 | xargs --null grep -L "LICENSE"\
	
#---------------------------------------------------------------------------------------
# Cleaning up
	
clean:
	$(RM) -rf $(BINDIR)
	$(RM) -rf $(OUTDIR)
	make -C $(DOCDIR)  clean
	make -C $(TESTDIR) clean
	
#---------------------------------------------------------------------------------------
# OLD
	
# CGIDIR = ideas.cs.uu.nl:/var/www/cgi-bin/
# WEBDIR = ideas.cs.uu.nl:/var/www/html/genexas/

# cgi-install: laservice
# 	scp $(BINDIR)/laservice.cgi $(CGIDIR)

# web:	
#	scp -r src/Presentation/genexas/* $(WEBDIR)