# SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
#
# SPDX-License-Identifier: MIT

TEST_DIR = test

UNIFFI_GENERATED_FILES = $(TEST_DIR)/libtezos.so $(TEST_DIR)/tezos.py

.PHONY: clean
clean:
	@rm -f $(UNIFFI_GENERATED_FILES)

.PHONY: test
test: $(UNIFFI_GENERATED_FILES)
	@pytest $(TEST_DIR) -v
