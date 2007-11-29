.PHONY: all checks solver doc run report markup clean

# On Windows machines (under cygwin), ghc automatically puts the .exe suffix after
# all executables it generates. This gives some problems when we have to call this
# executable later on. Hence, we first test whether we are on a windows machine or 
# not (check whether Win32 is listed under ghc's installed packages).
#
# on Windows: EXE = .exe
# otherwise:  EXE = 
EXE = $(shell ghc-pkg list | grep -q Win32 && echo .exe)

default: solver

all: solver doc markup

SOURCES = src/*.hs src/Logic/*.hs src/Logic/Solver/*.hs src/Matrix/*.hs

solver: bin/solver$(EXE)

bin/solver$(EXE): $(SOURCES)
	ghc --make -O -isrc -odir out -hidir out -o bin/solver$(EXE) src/Main.hs

checks:
	cd src; runhaskell Checks.hs; cd ..

run:
	ghci -isrc -odir out -hidir out src/Main.hs

# Cygwin only
runwin:
	ghcii.sh -isrc -odir out -hidir out src/Main.hs
	
doc: doc/index.html

doc/index.html: $(SOURCES) doc/prologue
	haddock -o doc -s "../%F" -p doc/prologue --html $(SOURCES)

hpc/bin/solver$(EXE): $(SOURCES)
	ghc -fhpc --make -hpcdir hpc/out -isrc -odir hpc/out -hidir hpc/out -o hpc/bin/solver$(EXE) src/Checks.hs
	
hpc/bin/solver.tix: hpc/bin/solver$(EXE)
	cd hpc/bin; rm -f solver.tix; ./solver$(EXE); cd ../..
	
report: hpc/bin/solver.tix
	hpc report -hpcdir=hpc/out hpc/bin/solver$(EXE)

markup: hpc/doc/hpc_index.html

hpc/doc/hpc_index.html: hpc/bin/solver.tix
	hpc markup --hpcdir=hpc/out --destdir=hpc/doc hpc/bin/solver$(EXE)
	
clean:
	rm -rf bin/*
	rm -rf out/*
	rm -rf doc/*.html
	rm -rf doc/*.gif
	rm -rf doc/haddock.*
	rm -rf hpc/bin/*
	rm -rf hpc/out/*
	rm -rf hpc/doc/*