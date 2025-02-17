# SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
#
# SPDX-License-Identifier: MIT

.PHONY: all
all: check

TARGET=rv64-inline-asm-tests

.PHONY: build
build:
	@cargo build --release
	@cp target/riscv64gc-unknown-none-elf/release/$(TARGET) $(TARGET)

.PHONY: check
check:
	@exec ../../scripts/format.sh --check
	@cargo doc --all-features --document-private-items --no-deps

.PHONY: clean
clean:
	@cargo clean
	@rm -f $(TARGET)
