SUBDIRS	:= bench src test tools
SOURCES	:= $(shell find $(SUBDIRS) -type f) dist/setup-config
ifneq ($(shell which nproc),)
PROCS	:= $(shell nproc)
else
PROCS	:= $(shell sysctl -n hw.ncpu)
endif


.PHONY: run
run: dist/build/pokemon/pokemon
	cabal run pokemon --jobs=$(PROCS)

.PHONY: bench
bench: dist/build/pokemon/pokemon
	cabal bench --jobs=$(PROCS)

.PHONY: check
check: test
	hlint $(SUBDIRS)
	stylish-haskell -i $(shell find $(SUBDIRS) -name "*.hs")
	git diff --exit-code --ignore-submodules

.PHONY: test
test: dist/build/pokemon/pokemon
	cabal test --jobs=$(PROCS) | grep -v '^Writing: '

dist/build/pokemon/pokemon: $(SOURCES)
	cabal build --jobs=$(PROCS)

dist/setup-config: pokemon.cabal protos/src/Pokemon.proto src/encrypt.c src/encrypt_clean.c
	cabal update
	cabal install --only-dependencies --enable-tests --enable-benchmarks
	cabal configure --disable-profiling --enable-tests --enable-benchmarks

protos/src/Pokemon.proto: $(shell find protos/src/POGOProtos -name "*.proto")
	echo 'syntax = "proto3";' > $@
	(for i in $^; do cat $$i; echo; done) \
		| egrep -v '^(syntax|package|import)' \
		| sed -e 's/\.POGOProtos\.[^ ]*\.//g' \
		>> $@

src/encrypt.c:
	curl http://pgoapi.com/pgoencrypt.tar.gz \
		| tar zx --strip-components 1 "pgoencrypt/src/encrypt.c"

src/encrypt_clean.c: src/simplify src/encrypt.c
	$+ $@
