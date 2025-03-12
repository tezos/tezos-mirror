# SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
#
# SPDX-License-Identifier: MIT

TEZOS_PACKAGE = org/example/tezos
RESOURCES_DIR = app/src/main/resources
LIB_DIR = app/src/main/kotlin/$(TEZOS_PACKAGE)
UNIFFI_DIR = ../uniffi_generated_files

link-lib: $(UNIFFI_DIR)/libtezos.so $(UNIFFI_DIR)/$(TEZOS_PACKAGE)/tezos.kt
	@ln -f $(UNIFFI_DIR)/libtezos.so $(RESOURCES_DIR)/libtezos.so
	@ln -f $(UNIFFI_DIR)/$(TEZOS_PACKAGE)/tezos.kt $(LIB_DIR)/tezos.kt

.PHONY: build
build: link-lib \
	$(RESOURCES_DIR)/libtezos.so \
	$(LIB_DIR)/tezos.kt
	@./gradlew build

.PHONY: test
test: build
	@./gradlew test

.PHONY: clean
clean:
	@./gradlew clean
	@rm -f $(RESOURCES_DIR)/libtezos.so $(LIB_DIR)/tezos.kt
