build: hbuild sbuild

hbuild: dist/build/site/site

dist/build/site/site: src/Main.hs
	cabal build

sbuild:
	dist/build/site/site build

clean: sclean hclean

sclean:
	dist/build/site/site clean

deploy: build
	dist/build/site/site deploy

hclean:
	cabal clean

