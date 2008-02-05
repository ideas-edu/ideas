.PHONY: all checks solver doc run report markup clean test

# On Windows machines (under cygwin), ghc automatically puts the .exe suffix after
# all executables it generates. This gives some problems when we have to call this
# executable later on. Hence, we first test whether we are on a windows machine or 
# not (check whether Win32 is listed under ghc's installed packages).
#
# on Windows: EXE = .exe
# otherwise:  EXE = 

WINDOWS = $(shell ghc-pkg list | grep -q Win32 && echo yes || echo no)

ifeq ($(WINDOWS), yes)
EXE  = .exe
GHCI = ghcii.sh
else
EXE  = 
GHCI = ghci
endif

# Define directories to store results
BINDIR  = bin
OUTDIR  = out
DOCDIR  = doc
HPCDIR  = hpc
CGIDIR  = ideas.cs.uu.nl:/var/www/cgi-bin/

default: solver

test:
	# Windows OS: $(WINDOWS) 
	# Executable suffix: $(EXE) 
	# GHC interpreter: $(GHCI)

all: solver solvergui wiki docs markup cgi

SOURCES = src/Common/*.hs src/Domain/*.hs src/Domain/Logic/*.hs \
	  src/Domain/Logic/Solver/*.hs src/Domain/LinearAlgebra/*.hs \
	  src/Presentation/ExerciseAssistant/*.hs src/OpenMath/*.hs \
	  src/Domain/Fraction/*.hs

solver: bin/solver$(EXE)

$(BINDIR):
	mkdir -p $(BINDIR)

$(OUTDIR):
	mkdir -p $(OUTDIR)

$(DOCDIR):
	mkdir -p $(DOCDIR)
	mkdir -p $(DOCDIR)/Wiki
		
bin/solver$(EXE): $(BINDIR) $(OUTDIR) $(SOURCES)
	ghc --make -O -isrc -odir out -hidir out -o bin/solver$(EXE) src/Common/Main.hs

solvergui: bin/solvergui$(EXE)

bin/solvergui$(EXE): $(BINDIR) $(OUTDIR) $(SOURCES) src/Presentation/ExerciseAssistant/ExerciseAssistant.hs src/Presentation/ExerciseAssistant/exerciseassistant.glade src/Presentation/ExerciseAssistant/exerciseassistant.gladep
	ghc --make -O -isrc -isrc/Presentation/ExerciseAssistant -odir out -hidir out -o bin/solvergui$(EXE) src/Presentation/ExerciseAssistant/ExerciseAssistant.hs
	cp src/Presentation/ExerciseAssistant/exerciseassistant.glade bin/

wiki: $(DOCDIR) $(BINDIR)/MakeWikiPages$(EXE) 
	$(BINDIR)/MakeWikiPages$(EXE) $(DOCDIR)/Wiki

$(BINDIR)/MakeWikiPages$(EXE): $(SOURCES)
	ghc --make -O -isrc -odir out -hidir out -o bin/MakeWikiPages$(EXE) src/OpenMath/StrategyToWiki.hs

checks: $(BINDIR) $(OUTDIR)
	cd src/Common; runhaskell -i.. Checks.hs; cd ..

run: $(BINDIR) $(OUTDIR)
	$(GHCI) -isrc -odir out -hidir out 

docs:	doc/index.html

doc/index.html: $(DOCDIR) $(SOURCES) src/Common/prologue
	haddock -o doc -s "../%F" -p src/Common/prologue --html $(SOURCES)

hpc/bin/solver$(EXE): $(SOURCES)
	mkdir -p $(HPCDIR)/out $(HPCDIR)/bin
	ghc -fhpc --make -hpcdir hpc/out -isrc -odir hpc/out -hidir hpc/out -o hpc/bin/solver$(EXE) src/Common/Checks.hs

hpc/bin/solver.tix: hpc/bin/solver$(EXE)
	cd hpc/bin; rm -f solver.tix; ./solver$(EXE); cd ../..

report: hpc/bin/solver.tix
	hpc report --hpcdir=hpc/out hpc/bin/solver$(EXE)

markup: hpc/doc/hpc_index.html

hpc/doc/hpc_index.html: hpc/bin/solver.tix
	mkdir -p $(HPCDIR)/doc
	hpc markup --hpcdir=hpc/out --destdir=hpc/doc hpc/bin/solver$(EXE)

cgi:	$(BINDIR)/laservice.cgi

$(BINDIR)/laservice.cgi: $(BINDIR) $(OUTDIR) $(SOURCES)
	ghc --make -O -isrc -odir $(OUTDIR) -hidir $(OUTDIR) -o $(BINDIR)/laservice.cgi src/OpenMath/Main.hs

cgi-install: $(BINDIR)/laservice.cgi
	scp $(BINDIR)/laservice.cgi $(CGIDIR)

clean:
	rm -rf bin
	rm -rf out
	rm -rf doc
	rm -rf hpc
