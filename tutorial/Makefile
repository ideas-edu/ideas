default: Tutorial.html Tutorial-solution.html Tutorial.exe

IDEASDIR   = ../../tags/ideas-1.2
SRCDIR     = $(IDEASDIR)/src
OUTDIR     = $(IDEASDIR)/out
INSTALLDIR = /var/www/tutorial

ghci:
	ghci -i$(SRCDIR) -odir$(OUTDIR) -hidir$(OUTDIR) -Wall -fwarn-tabs

install:
	cp Tutorial.html $(INSTALLDIR)/index.html
	cp Tutorial-solution.html $(INSTALLDIR)/Tutorial-solution.html
	cp Tutorial.css $(INSTALLDIR)
	cp ideas.png $(INSTALLDIR)
	cp ounl.png $(INSTALLDIR)

%.html: %.lhs
	pandoc --from markdown+lhs --to html -o $@ $<

Tutorial.exe:
	ghc --make -i../src -hidir ../out -odir ../out Tutorial.lhs -o Tutorial.exe
