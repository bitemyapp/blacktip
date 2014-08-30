install-with-test-support:
	cabal install --enable-tests
	cabal test

test:
	cabal clean
	cabal test

build:
	cabal build

test-repl:
	cabal repl tests

repl:
	cabal repl

reset:
	rm -rf .cabal-sandbox
	cabal sandbox init


# Generate haddock
# THIS DOES NOT WORK IN A MAKEFILE!
# cabal haddock --hyperlink-source --html-location='http://hackage.haskell.org/package/$pkg/docs' --contents-location='http://hackage.haskell.org/package/$pkg'

# export BTVER=0.1.0.0

# upload:
# 	cabal sdist
# 	cabal upload dist/blacktip-$BTVER.tar.gz

# Make docs
# Invoke haddock
# cp -R ./dist/doc/html/blacktip/ blacktip-$BTVER-docs
# tar cvz --format=ustar -f blacktip-$BTVER-docs.tar.gz blacktip-$BTVER-docs
# curl -X PUT -H 'Content-Type: application/x-tar' -H 'Content-Encoding: gzip' --data-binary "@blacktip-$BTVER-docs.tar.gz" "https://bitemyapp:$PASSWORD@hackage.haskell.org/package/blacktip-$BTVER/docs"
