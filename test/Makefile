default: tests
all: tests

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

tests: $(ALL-OUT)
	# --- Unit Tests ------------------------
	@for i in $(ALL-WITHOUT); do \
	  echo $$i; \
	  if $(WINDOWS)==no; then dos2unix $$i.exp &> /dev/null; fi; \
	  diff -I "version=" $$i.out $$i.exp; \
	done;

mathdox-request/%.out: mathdox-request/%.txt $(BINDIR)/service.cgi
	$(BINDIR)/service.cgi --file $< > $@

json-rpc/%.out: json-rpc/%.json $(BINDIR)/service.cgi
	$(BINDIR)/service.cgi --file $< > $@

xml-request/%.out: xml-request/%.xml $(BINDIR)/service.cgi
	$(BINDIR)/service.cgi --file $< > $@
	
%.exp: %.out
	cp $^ $@