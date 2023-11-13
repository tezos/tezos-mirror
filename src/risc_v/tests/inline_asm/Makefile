# SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
#
# SPDX-License-Identifier: MIT

.PHONY: all
all: build-deps check

TARGET=rv64-inline-asm-tests

.PHONY: build
build:
	@cargo build --release
	@cp target/riscv64gc-unknown-none-elf/release/$(TARGET) $(TARGET)

.PHONY: build-deps
build-deps:
	@rustup target add riscv64gc-unknown-none-elf

.PHONY: check
check:
	@cargo fmt --check

.PHONY: clean
clean:
	@cargo clean
	@rm -f $(TARGET)
