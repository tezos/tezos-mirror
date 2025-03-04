# SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
#
# SPDX-License-Identifier: MIT

RESOURCES_DIR = app/src/main/resources
LIB_DIR = app/src/main/kotlin/org/example/tezos

.PHONY: build
build: $(RESOURCES_DIR)/libtezos.so $(LIB_DIR)/tezos.kt
	@./gradlew build

.PHONY: clean
clean:
	@./gradlew clean
	@rm -f $(RESOURCES_DIR)/libtezos.so
	@rm -rf $(LIB_DIR)
