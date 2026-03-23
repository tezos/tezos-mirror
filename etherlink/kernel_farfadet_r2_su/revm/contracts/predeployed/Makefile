SRC := $(wildcard *.sol)
BYTECODE := $(SRC:.sol=.bin)
FORGE_WITNESS := _build/.forge

$(FORGE_WITNESS): $(SRC) Makefile
	@forge build --via-ir $(shell pwd) --out $(shell pwd)/_build --no-cache --force --use 0.8.29
	@touch $@

%.bin: %.sol $(FORGE_WITNESS)
	@cat _build/$</*.json \
		| jq '.deployedBytecode.object' -r \
		| sed 's/^0x//' \
		| xxd -r -p \
		> $@

.PHONY: bytecode
bytecode: $(BYTECODE)
