# SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
#
# SPDX-License-Identifier: MIT

.PHONY: check
check:
	@make -C rust/ check

.PHONY: bindings
bindings:
	@make -C rust/ generate-bindings

.PHONY: clean
clean:
	@make -C rust/ clean
	@make -C python/ clean

.PHONY: test
test:
	@make -C rust/ test
