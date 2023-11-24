# SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
#
# SPDX-License-Identifier: MIT

.PHONY: all
all: build test check

NATIVE_TARGET ?=
ifneq ($(NATIVE_TARGET),)
NATIVE_OPT := --target="$(NATIVE_TARGET)"
endif

.PHONY: build
build:
	@cargo build --release $(NATIVE_OPT)

.PHONY: build-deps
build-deps:
ifneq ($(NATIVE_TARGET),)
	@rustup target add $(NATIVE_TARGET)
endif

.PHONY: test
test:
	@cargo test

.PHONY: check
check:
	@cargo clippy --all-targets -- --deny warnings

.PHONY: clean
clean:
	@cargo clean
