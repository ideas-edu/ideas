default: service ideas
all: binaries unit-tests documentation

SRCDIR = src

VERSION = 0.5.0

include Makefile.incl

binaries: service laservice viewlog solvergui ideas

service: $(BINDIR)/service.cgi
laservice: $(BINDIR)/laservice.cgi
viewlog: $(BINDIR)/viewlog.cgi
solvergui: $(BINDIR)/solvergui$(EXE)
ideas: $(BINDIR)/ideasWX$(EXE)

$(BINDIR)/service.cgi: $(HS-SOURCES) revision
	$(MKDIR) -p $(BINDIR) $(OUTDIR)
	$(GHC) $(GHCFLAGS) -o $@ src/Service/Main.hs
	$(STRIP) $@

$(BINDIR)/laservice.cgi: $(HS-SOURCES) revision
	$(MKDIR) -p $(BINDIR) $(OUTDIR)
	$(GHC) $(GHCFLAGS) -o $@ src/OpenMath/Main.hs
	$(STRIP) $@

$(BINDIR)/viewlog.cgi: $(HS-SOURCES) revision
	$(MKDIR) -p $(BINDIR) $(OUTDIR)
	$(GHC) $(GHCFLAGS) -isrc/Presentation -o $@ src/Presentation/ViewLog/Main.hs
	$(STRIP) $@

$(BINDIR)/solvergui$(EXE): $(HS-SOURCES) $(GLADE-SOURCES) revision
ifeq ($(GTK), yes)
	$(MKDIR) -p $(BINDIR) $(OUTDIR)
	$(GHC) $(GHCFLAGS) $(GHCGUIFLAGS) -isrc/Presentation/ExerciseAssistant -o $@ src/Presentation/ExerciseAssistant/ExerciseAssistant.hs
	$(STRIP) $@
	$(CP) src/Presentation/ExerciseAssistant/exerciseassistant.glade bin/
	$(CP) src/Presentation/ExerciseAssistant/ounl.jpg bin/	
endif

$(BINDIR)/ideasWX$(EXE): $(BINDIR)/ounl.jpg $(HS-SOURCES) src/Presentation/ExerciseAssistant/IdeasWX.hs revision
ifeq ($(WX), yes)
	$(MKDIR) -p $(BINDIR) $(OUTDIR)
	$(GHC) $(GHCFLAGS) $(GHCGUIFLAGS) -isrc/Presentation/ExerciseAssistant -o $@ src/Presentation/ExerciseAssistant/IdeasWX.hs
	$(STRIP) $@
ifeq ($(WINDOWS), no)
	$(CD) $(BINDIR); $(MAC) ideasWX
endif
endif

$(BINDIR)/ounl.jpg: $(SRCDIR)/Presentation/ExerciseAssistant/ounl.jpg
	$(MKDIR) -p $(BINDIR)
	$(CP) $< $@

#---------------------------------------------------------------------------------------
# Other directories

documentation: doc

$(DOCDIR): $(HS-SOURCES)	
	make -C $(DOCDIR) || exit 1
	$(TOUCH) $(DOCDIR) # To get a timestamp

unit-tests: $(TESTDIR)/test.log
test: $(TESTDIR)/test.log

$(TESTDIR)/test.log: $(HS-SOURCES) $(BINDIR)/service.cgi
	make -C $(TESTDIR) || exit 1

#---------------------------------------------------------------------------------------
# Helper targets

ghci: revision
	$(MKDIR) -p $(OUTDIR)
	$(GHCI) -i$(SRCDIR) -i$(SRCDIR)/Presentation -i$(SRCDIR)/Presentation/ExerciseAssistant -i$(SRCDIR)/Presentation/ExerciseDoc -odir $(OUTDIR) -hidir $(OUTDIR) $(GHCWARN)

run: ideas
ifeq ($(WINDOWS), yes)
	$(BINDIR)/ideasWX$(EXE)
else
	open $(BINDIR)/ideasWX.app/
endif

revision: $(SRCDIR)/Service/Revision.hs

$(SRCDIR)/Service/Revision.hs: $(filter-out $(SRCDIR)/Service/Revision.hs, $(HS-SOURCES))
	echo "module Service.Revision where" > $@
	echo 'version = "$(VERSION)"' >> $@
ifeq ($(SVN), yes)
	svn info | grep 'Revision' | sed 's/.*\: /revision = /' >> $@
	svn info | grep 'Last Changed Date' | sed 's/.*(\(.*\))/lastChanged = \"\1\"/' >> $@
else
	echo 'revision = 0' >> $@
	echo 'lastChanged = "unknown"' >> $@
endif

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