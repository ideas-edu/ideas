OVERVIEWSOURCES = $(wildcard */*/overview.lhs)
OVERVIEWTARGETS = $(patsubst %.lhs,%.pdf, $(OVERVIEWSOURCES))
RULESOURCES     = $(wildcard */*/rule*.lhs)
RULETARGETS     = $(patsubst %.lhs,%.png, $(RULESOURCES))

default: $(OVERVIEWTARGETS) $(RULETARGETS) clean-tmp
all: $(OVERVIEWTARGETS) $(RULETARGETS) clean-tmp

include ../../Makefile.incl

#---------------------------------------------------------------------------------------
# Cleaning up

clean: clean-tmp
	$(RM) -f */*/*.pdf
	$(RM) -f */*/*.png
	$(RMDIR) */* >& /dev/null || exit 0
	
clean-tmp:
	$(RM) -f */*/*.aux
	$(RM) -f */*/*.ptb
	$(RM) -f */*/*.log
	$(RM) -f */*/*.lhs