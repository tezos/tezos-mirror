SRC := $(wildcard *.sol)
BYTECODE := $(SRC:.sol=.bin)
FORGE_WITNESS := _build/.forge
REQUIRED_FORGE_VERSION := 1.5.0

# Different versions of forge can compile the same solidity contract
# differently; we force a specific version of forge because the EVM
# output is part of the kernel.
check-forge:
	@forge --version | grep -q "$(REQUIRED_FORGE_VERSION)" || \
	  (echo "ERROR: forge $(REQUIRED_FORGE_VERSION) required. (Hint: use 'foundryup -i $(REQUIRED_FORGE_VERSION)')"; exit 1)

$(FORGE_WITNESS): check-forge $(SRC) Makefile
	@mkdir -p _build
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
