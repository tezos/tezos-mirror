# SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
#
# SPDX-License-Identifier: MIT

.PHONY: all
all: build test check

.PHONY: build
build:
	@make -C dummy_kernel build
	@make -C jstz build

.PHONY: build-deps
build-deps:
	@exec ./install-rust-std-hermit.sh
	@rustup component add rustfmt clippy

.PHONY: test
test:
	@make -C dummy_kernel test
	@make -C jstz build

.PHONY: check
check:
	@make -C dummy_kernel check
	@make -C jstz check

.PHONY: clean
clean:
	@make -C dummy_kernel clean
	@make -C jstz clean
