default: QuickTest.exe

HS-SOURCES = $(wildcard *.hs)

QuickTest.exe: $(HS-SOURCES)
	ghc --make QuickTest

mac: QuickTest.exe
	macosx-app -v QuickTest
	echo "#!/bin/bash\n\nopen -a QuickTest.app --args mac.cfg\n" > QuickTest

clean:
	rm -fr *.o *.hi QuickTest.app QuickTest
