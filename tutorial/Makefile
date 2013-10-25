default: Tutorial.html

ghci:
	ghci -i../src -odir../out -hidir../out -Wall -fwarn-tabs

%.html: %.lhs
	pandoc --from markdown+lhs --to html --css=Tutorial.css -o $@ $<