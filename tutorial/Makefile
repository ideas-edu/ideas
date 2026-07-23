default: tutorial Tutorial.html Tutorial-solution.html

tutorial:
	cabal install --installdir=. --overwrite-policy=always

%.html: %.lhs 
	pandoc --from markdown+lhs --to html --template=template.html -o $@ $<

ghci:
	cabal repl	

docker:
	docker build -t ideas-tutorial:latest .
	
server:
	docker run --rm -it -p 8080:80 ideas-tutorial:latest

clean:
	cabal clean
	rm -f Tutorial.html Tutorial-solution.html tutorial
	docker rmi ideas-tutorial:latest
