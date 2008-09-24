FMTSOURCES = $(wildcard *.fmt)
LHSSOURCES = $(wildcard *.lhs)
PDFTARGETS = $(patsubst %.lhs,%.pdf,$(LHSSOURCES))

default: $(PDFTARGETS)
all: $(PDFTARGETS)

include ../../Makefile.incl

#---------------------------------------------------------------------------------------
# Cleaning up

clean:
	$(RM) -f *.pdf 
	$(RM) -f *.lhs 
	$(RM) -f *.tex 
	$(RM) -f *.ptb
	$(RM) -f *.aux
	$(RM) -f *.log