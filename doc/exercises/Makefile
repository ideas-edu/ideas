OVERVIEWSOURCES = $(wildcard */*/overview.lhs)
OVERVIEWTARGETS = $(patsubst %.lhs,%.pdf, $(OVERVIEWSOURCES))
RULESOURCES     = $(wildcard */*/rule*.lhs)
RULETARGETS     = $(patsubst %.lhs,%.png, $(RULESOURCES))

default: $(OVERVIEWTARGETS) $(RULETARGETS)
all: $(OVERVIEWTARGETS) $(RULETARGETS)

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