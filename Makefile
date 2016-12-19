all: build deploy

build: hbuild sbuild

hbuild: dist/build/site/site

dist/build/site/site: src/Main.hs
	cabal build

sbuild:
	cabal run build

clean: sclean hclean

sclean:
	cabal run clean

deploy: build
	cabal run deploy

hclean:
	cabal clean

