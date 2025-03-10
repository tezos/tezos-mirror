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

.PHONY: generate-python
generate-python: build
	@cargo run --features uniffi/cli --bin uniffi-bindgen -- generate --language python --out-dir ../python/test --library target/release/libtezos.so
	@cp target/release/libtezos.so ../python/test/libtezos.so

.PHONY: generate-bindings
generate-bindings: generate-python

.PHONY: clean
clean:
	@cargo clean

.PHONY: test
test:
	@cargo test
