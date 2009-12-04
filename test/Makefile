default: test.log
all: test-all.log

include ../Makefile.incl

BINDIR = ../bin

TESTS1 = $(wildcard mathdox-request/*.txt)
OUTS1  = $(patsubst %.txt,%.out,$(TESTS1))

TESTS2 = $(wildcard json-rpc/*.json)
OUTS2  = $(patsubst %.json,%.out,$(TESTS2))

TESTS3 = $(wildcard xml-request/*.xml)
OUTS3  = $(patsubst %.xml,%.out,$(TESTS3))

TESTS4 = $(wildcard dwo-derivations/*.xml)
OUTS4  = $(patsubst %.xml,%.out,$(TESTS4))

SOME-OUT     = $(OUTS1) $(OUTS2) $(OUTS3)
SOME-WITHOUT = $(patsubst %.out,%,$(SOME-OUT))

ALL-OUT     = $(OUTS1) $(OUTS2) $(OUTS3) $(OUTS4)
ALL-EXP     = $(patsubst %.out,%.exp,$(ALL-OUT))
ALL-WITHOUT = $(patsubst %.out,%,$(ALL-OUT))

#---------------------------------------------------------------------------------------
# Testing

test.log: $(SOME-OUT)
	# --- Unit Tests ------------------------
	@for i in $(SOME-WITHOUT); do \
	  echo $$i; \
	  echo $$i >> test.log; \
	  dos2unix $$i.exp &> /dev/null; \
	  diff -I "version" $$i.out $$i.exp; \
	  diff -I "version" $$i.out $$i.exp >> test.log; \
	done;

test-all.log: $(ALL-OUT)
	# --- Unit Tests ------------------------
	@for i in $(ALL-WITHOUT); do \
	  echo $$i; \
	  echo $$i >> test-all.log; \
	  dos2unix $$i.exp &> /dev/null; \
	  diff -I "version" $$i.out $$i.exp; \
	  diff -I "version" $$i.out $$i.exp >> test-all.log; \
	done;

mathdox-request/%.out: mathdox-request/%.txt $(BINDIR)/ideas.cgi
	$(BINDIR)/ideas.cgi --file $< > $@

json-rpc/%.out: json-rpc/%.json $(BINDIR)/ideas.cgi
	$(BINDIR)/ideas.cgi --file $< > $@

xml-request/%.out: xml-request/%.xml $(BINDIR)/ideas.cgi
	$(BINDIR)/ideas.cgi --file $< > $@

dwo-derivations/%.out: dwo-derivations/%.xml $(BINDIR)/ideas.cgi
	$(BINDIR)/ideas.cgi --file $< > $@
	
exp: $(ALL-EXP)
	
%.exp: %.out
	cp $^ $@
	
#---------------------------------------------------------------------------------------
# Cleaning up

clean:
	$(RM) -f test.log
	$(RM) -f *.out
	$(RM) -f */*.out