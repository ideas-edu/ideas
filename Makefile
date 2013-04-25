default: ideas
all: ideas haddock hlint sdist

VERSION = 1.0.11

#---------------------------------------------------------------------------------------
# Applications, directories

CABAL   = cabal
GHC     = ghc
GHCI    = ghci
HADDOCK = haddock
HLINT   = hlint
RM      = rm

GHCWARN = -Wall -fwarn-tabs

SRCDIR     = src
OUTDIR     = out
DOCSDIR    = docs
HADDOCKDIR = $(DOCSDIR)/haddock

HS-SOURCES = $(wildcard $(SRCDIR)/*/*.hs $(SRCDIR)/*/*/*.hs $(SRCDIR)/*/*/*/*.hs $(SRCDIR)/*/*/*/*/*.hs)

#---------------------------------------------------------------------------------------
# GHC

ghci:
	$(GHCI) -i$(SRCDIR) -odir $(OUTDIR) -hidir $(OUTDIR) $(GHCWARN)

ideas:
	$(GHC) -i$(SRCDIR) -odir $(OUTDIR) -hidir $(OUTDIR) $(GHCWARN) $(HS-SOURCES) 2>&1 | tee $(DOCSDIR)/compile.txt

#---------------------------------------------------------------------------------------
# Documentation

haddock:
	$(HADDOCK) --html -o $(HADDOCKDIR) --prologue=$(DOCSDIR)/prologue --title="Ideas: feedback services for intelligent tutoring systems" $(HS-SOURCES)
	
hlint:
	$(HLINT) --report=$(DOCSDIR)/hlint.html $(HS-SOURCES) | exit 0

#---------------------------------------------------------------------------------------
# Cabal targets

configure:
	$(CABAL) configure

build:
	$(CABAL) build

install:
	$(CABAL) install

sdist: configure
	$(CABAL) sdist

#---------------------------------------------------------------------------------------
# Cleaning up

clean:
	$(CABAL) clean
	$(RM) -rf out
	$(RM) -rf $(DOCSDIR)/hlint.html
	$(RM) -rf $(DOCSDIR)/compile.txt
	$(RM) -rf $(HADDOCKDIR)
	
#---------------------------------------------------------------------------------------
# Misc

nolicense:
	find src -name \*.hs -print0 | xargs --null grep -L "LICENSE"