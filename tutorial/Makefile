default: Tutorial.html

ghci:
	ghci -i../src -odir../out -hidir../out -Wall -fwarn-tabs

%.html: %.lhs
	pandoc --standalone --from markdown+lhs --to html --highlight-style tango -o $@ $<