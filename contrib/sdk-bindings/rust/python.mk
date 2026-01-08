# SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
#
# SPDX-License-Identifier: MIT

.PHONY: build
build:
	@maturin build

.PHONY: clean
clean:

.PHONY: test
test:
	@maturin develop
	@pytest python-tests/ -v

.PHONY: publish
publish:
	@maturin upload target/wheels/*
