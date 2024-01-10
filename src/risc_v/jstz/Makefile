# SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
#
# SPDX-License-Identifier: MIT

NATIVE_TARGET ?=
ifneq ($(NATIVE_TARGET),)
NATIVE_OPT := --target "$(NATIVE_TARGET)"
endif

.PHONY: all
all: build test check

.PHONY: build
build:
	@cargo build --release

.PHONY: test
test:
	@cargo test --no-default-features $(NATIVE_OPT)

.PHONY: run
run:
	@cargo run --no-default-features

.PHONY: check
check:
	@cargo fmt --check
	# TODO: Clippy! (currently doesn't work)

.PHONY: clean
clean:
	@cargo clean
