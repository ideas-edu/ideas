default: test-all.log
all: test-all.log

include ../Makefile.incl

BINDIR = ../bin

# -- JSON 
TESTS1 = $(wildcard */*.json)
OUTS1  = $(patsubst %.json,%.out,$(TESTS1))

# -- XML
TESTS2 = $(wildcard */*.xml)
OUTS2  = $(patsubst %.xml,%.out,$(TESTS2))

# -- XML dwo-derivations
TESTS3 = $(wildcard dwo-derivations/*/*.xml)
OUTS3  = $(patsubst %.xml,%.out,$(TESTS3))

SOME-OUT     = $(OUTS1) $(OUTS2)
SOME-WITHOUT = $(patsubst %.out,%,$(SOME-OUT))

ALL-OUT     = $(OUTS1) $(OUTS2) $(OUTS3)
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

%.out: %.xml $(BINDIR)/ideas.cgi
	$(BINDIR)/ideas.cgi --fixed-rng --file $< > $@

%.out: %.json $(BINDIR)/ideas.cgi
	$(BINDIR)/ideas.cgi --fixed-rng --file $< > $@

exp: $(ALL-EXP)
	
%.exp: %.out
	cp $^ $@
	
#---------------------------------------------------------------------------------------
# Cleaning up

clean:
	$(RM) -f test.log
	$(RM) -f *.out
	$(RM) -f */*.out