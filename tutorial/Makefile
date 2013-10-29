default: Tutorial.html

IDEASDIR   = ../../tags/ideas-1.1
SRCDIR     = $(IDEASDIR)/src
OUTDIR     = $(IDEASDIR)/out
INSTALLDIR = /var/www/tutorial

ghci:
	ghci -i$(SRCDIR) -odir$(OUTDIR) -hidir$(OUTDIR) -Wall -fwarn-tabs

install:
	cp Tutorial.html $(INSTALLDIR)/index.html
	cp Tutorial.css $(INSTALLDIR)
	cp ideas.png $(INSTALLDIR)
	cp ounl.png $(INSTALLDIR)

%.html: %.lhs
	pandoc --from markdown+lhs --to html --css=Tutorial.css -o $@ $<