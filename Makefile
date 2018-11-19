all: build

build:
	ghc --make Main.hs
clean:
	rm -rf *.o *.hi Main
