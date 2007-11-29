.PHONY: run

default: laserver

%.hs: %.dtd
	DtdToHaskell $< > $@
	perl -i -pe 's/Extsubset/$(basename $@)/g' $@
	rm $@.bak

run:
	ghci -i../Bastiaan/src LAServer.hs

runwin:
	ghcii.sh -i../Bastiaan/src Exercises.hs
	
laserver:
	ghc --make -i../Bastiaan/src -O -o laserver LAServer.hs