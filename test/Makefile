default: test.log
all: test.log

include ../Makefile.incl

BINDIR = ../bin

TESTS1 = $(wildcard mathdox-request/*.txt)
OUTS1  = $(patsubst %.txt,%.out,$(TESTS1))

TESTS2 = $(wildcard json-rpc/*.json)
OUTS2  = $(patsubst %.json,%.out,$(TESTS2))

TESTS3 = $(wildcard xml-request/*.xml)
OUTS3  = $(patsubst %.xml,%.out,$(TESTS3))

ALL-OUT     = $(OUTS1) $(OUTS2) $(OUTS3)
ALL-EXP     = $(patsubst %.out,%.exp,$(ALL-OUT))
ALL-WITHOUT = $(patsubst %.out,%,$(ALL-OUT))

#---------------------------------------------------------------------------------------
# Testing

test.log: $(ALL-OUT)
	# --- Unit Tests ------------------------
	@for i in $(ALL-WITHOUT); do \
	  echo $$i; \
	  echo $$i >> test.log; \
	  if $(WINDOWS)==no; then dos2unix $$i.exp &> /dev/null; fi; \
	  diff -I "version" $$i.out $$i.exp; \
	  diff -I "version" $$i.out $$i.exp >> test.log; \
	done;

mathdox-request/%.out: mathdox-request/%.txt $(BINDIR)/service.cgi
	$(BINDIR)/service.cgi --file $< > $@

json-rpc/%.out: json-rpc/%.json $(BINDIR)/service.cgi
	$(BINDIR)/service.cgi --file $< > $@

xml-request/%.out: xml-request/%.xml $(BINDIR)/service.cgi
	$(BINDIR)/service.cgi --file $< > $@
	
%.exp: %.out
	cp $^ $@
	
#---------------------------------------------------------------------------------------
# Cleaning up

clean:
	$(RM) -f test.log
	$(RM) -f *.out
	$(RM) -f */*.out