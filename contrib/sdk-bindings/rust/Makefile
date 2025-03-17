# SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
#
# SPDX-License-Identifier: MIT

.PHONY: check
check:
	@cargo fmt --check
	@cargo check --locked --all-targets
	@cargo clippy --all-targets

.PHONY: build
build:
	@cargo build --release


TEZOS_LIB = target/release/libtezos.so
OUT_DIR = uniffi_generated_files

.PHONY: generate-%
generate-%: build
	@cargo run --features uniffi/cli --bin uniffi-bindgen -- generate --language $* --out-dir ../$*/$(OUT_DIR)/ --library $(TEZOS_LIB)
	@cp $(TEZOS_LIB) ../$*/$(OUT_DIR)/libtezos.so

.PHONY: generate-bindings
generate-bindings: generate-python generate-kotlin generate-swift

.PHONY: clean-%
clean-%:
	@rm -rf ../$*/$(OUT_DIR)

.PHONY: clean-bindings
clean-bindings: clean-python clean-kotlin clean-swift

.PHONY: clean
clean: clean-bindings
	@cargo clean

.PHONY: test
test:
	@cargo test
