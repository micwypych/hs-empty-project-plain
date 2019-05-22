.PHONY: run compile scripts

compile:
	cabal sandbox init
	cabal install --only-dependencies
	cabal build

run:
	rm *.db 2> /dev/null || echo "no db files to remove"
	cabal build
	cabal run

scripts:
	./create-student.sh
