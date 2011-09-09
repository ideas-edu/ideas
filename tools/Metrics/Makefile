default: Metrics.exe

.PHONY: Metrics.exe

Metrics.exe: 
	ghc --make -i../../src -hidir ../../out -odir ../../out -Wall Metrics.hs

ghci:
	ghcii.sh -i../../src -hidir ../../out -odir ../../out -Wall