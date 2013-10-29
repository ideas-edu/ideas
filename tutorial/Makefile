default: Tutorial.html

IDEASDIR = ../../tags/ideas-1.1
SRCDIR   = $(IDEASDIR)/src
OUTDIR   = $(IDEASDIR)/out

ghci:
	ghci -i$(SRCDIR) -odir$(OUTDIR) -hidir$(OUTDIR) -Wall -fwarn-tabs

%.html: %.lhs
	pandoc --from markdown+lhs --to html --css=Tutorial.css -o $@ $<