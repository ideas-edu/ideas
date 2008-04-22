.PHONY: all checks run report clean test

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

# Define directories to store results (assuming we'll always have `src')
BINDIR = bin
OUTDIR = out
DOCDIR = doc
HPCDIR = hpc
CGIDIR = ideas.cs.uu.nl:/var/www/cgi-bin/
WEBDIR = ideas.cs.uu.nl:/var/www/html/genexas/

FLAGS = --make -O -isrc -odir $(OUTDIR) -hidir $(OUTDIR)

default: solvergui

test:
# Windows OS: $(WINDOWS) 
# Executable suffix: $(EXE) 
# GHC interpreter: $(GHCI)

all: solvergui doc markup cgi service wikipages

SOURCES = src/Common/*.hs src/Service/*.hs src/Domain/*.hs src/Domain/Logic/*.hs \
	  src/Domain/RelationAlgebra/*.hs src/Domain/Fraction/*.hs \
	  src/Domain/Logic/Solver/*.hs src/Domain/LinearAlgebra/*.hs \
	  src/Presentation/ExerciseAssistant/*.hs src/OpenMath/*.hs

GLADE = src/Presentation/ExerciseAssistant/exerciseassistant.glade \
	src/Presentation/ExerciseAssistant/exerciseassistant.gladep

# AG: when used as a dependency it always fires, the time of the dir is set to the newest file in the (sub)dir
#$(BINDIR): 
#	mkdir -p $(BINDIR)

solvergui: $(BINDIR)/solvergui$(EXE)

$(BINDIR)/solvergui$(EXE): $(SOURCES) $(GLADE)	
#	if [ ! -d $(BINDIR) ] || [ ! -d $(OUTDIR) ]; then mkdir -p $(BINDIR) $(OUTDIR); fi  # AG: possible but overdone
	mkdir -p $(BINDIR) $(OUTDIR)
	ghc $(FLAGS) -isrc/Presentation/ExerciseAssistant -o $@ src/Presentation/ExerciseAssistant/ExerciseAssistant.hs
	cp src/Presentation/ExerciseAssistant/exerciseassistant.glade bin/
	cp src/Presentation/ExerciseAssistant/ounl.jpg bin/

checks: 
	cd src/Common; runhaskell -i.. Checks.hs; cd ..

run: solvergui
	$(BINDIR)/solvergui$(EXE)

ghci:
	mkdir -p $(OUTDIR)
	$(GHCI) -isrc -isrc/Presentation/ExerciseAssistant -odir $(OUTDIR) -hidir $(OUTDIR)

$(DOCDIR)/index.html: $(SOURCES) src/Common/prologue
	mkdir -p $(DOCDIR)
	haddock -o $(DOCDIR) -s "../%F" -p src/Common/prologue --html $(SOURCES)

doc: $(DOCDIR)/index.html

$(HPCDIR)/$(BINDIR)/solver$(EXE): $(SOURCES) 
	mkdir -p $(HPCDIR)/$(OUTDIR) $(HPCDIR)/$(BINDIR)
	ghc -fhpc --make -hpcdir $(HPCDIR)/$(OUTDIR) -isrc -odir $(HPCDIR)/$(OUTDIR) -hidir $(HPCDIR)/$(OUTDIR) \
	-o $@ src/Common/Checks.hs

$(HPCDIR)/$(BINDIR)/solver$(EXE).tix: $(HPCDIR)/$(BINDIR)/solver$(EXE)
	rm -f $(HPCDIR)/$(BINDIR)/solver$(EXE).tix; cd $(HPCDIR)/$(BINDIR); ./solver$(EXE); cd ../..

report: $(HPCDIR)/$(BINDIR)/solver$(EXE).tix
	hpc report --hpcdir=$(HPCDIR)/$(OUTDIR) $(HPCDIR)/$(BINDIR)/solver$(EXE)

markup: $(HPCDIR)/$(DOCDIR)/hpc_index.html

$(HPCDIR)/$(DOCDIR)/hpc_index.html: $(HPCDIR)/$(BINDIR)/solver$(EXE).tix
	mkdir -p $(HPCDIR)/$(DOCDIR)
	hpc markup --hpcdir=$(HPCDIR)/$(OUTDIR) --destdir=$(HPCDIR)/$(DOCDIR) $(HPCDIR)/$(BINDIR)/solver$(EXE)

$(BINDIR)/laservice.cgi: $(SOURCES)
	mkdir -p  $(BINDIR) $(OUTDIR)
	ghc $(FLAGS) -o $@ src/OpenMath/Main.hs

cgi: $(BINDIR)/laservice.cgi

cgi-install: cgi
	scp $(BINDIR)/laservice.cgi $(CGIDIR)

service: $(BINDIR)/service.cgi

$(BINDIR)/service.cgi: $(SOURCES)
	mkdir -p  $(BINDIR) $(OUTDIR)
	ghc $(FLAGS) -o $@ src/Service/Main.hs

wikipages: $(BINDIR)/wikipages$(EXE)

$(BINDIR)/wikipages$(EXE): $(SOURCES)
	mkdir -p $(BINDIR) $(OUTDIR) $(DOCDIR)
	ghc --make -O -isrc -odir $(OUTDIR) -hidir $(OUTDIR) -o $(BINDIR)/wikipages src/OpenMath/StrategyToWiki.hs
	$(BINDIR)/wikipages$(EXE) doc

web:	
	scp -r src/Presentation/genexas/* $(WEBDIR)

clean:
	rm -rf bin
	rm -rf out
	rm -rf doc
	rm -rf hpc
