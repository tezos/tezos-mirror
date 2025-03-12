# SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
#
# SPDX-License-Identifier: MIT

TEST_DIR = test
UNIFFI_DIR = uniffi_generated_files

link-lib: $(UNIFFI_DIR)/libtezos.so $(UNIFFI_DIR)/tezos.py
	@ln -f $(UNIFFI_DIR)/libtezos.so $(TEST_DIR)/libtezos.so
	@ln -f $(UNIFFI_DIR)/tezos.py $(TEST_DIR)/tezos.py

.PHONY: build
build: link-lib

.PHONY: clean
clean:
	@rm -f $(TEST_DIR)/libtezos.so $(TEST_DIR)/tezos.py

.PHONY: test
test: build \
	$(TEST_DIR)/libtezos.so \
	$(TEST_DIR)/tezos.py
	@pytest $(TEST_DIR) -v
