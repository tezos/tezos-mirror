# SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
#
# SPDX-License-Identifier: MIT

.PHONY: all
all: build-deps check

.PHONY: build
build:
	@make -C inline_asm build

.PHONY: build-deps
build-deps:
	@make -C inline_asm build-deps

.PHONY: check
check:
	@make -C inline_asm check

.PHONY: clean
clean:
	@make -C inline_asm clean
