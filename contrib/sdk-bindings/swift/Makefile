# SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
#
# SPDX-License-Identifier: MIT

UNIFFI_DIR = uniffi_generated_files
UNIFFI_BINDINGS_DIR = uniffi_bindings
LIB_DIR = tezos
RESOURCES_DIR = resources

TEST_DIR = Tests
BUILD_DIR = .build
TEST_EXE = $(BUILD_DIR)/test.exe

$(BUILD_DIR):
	@mkdir -p $(BUILD_DIR)

link-lib: $(UNIFFI_DIR)/tezos.swift \
	$(UNIFFI_DIR)/tezosFFI.h \
	$(UNIFFI_DIR)/tezosFFI.modulemap \
	$(UNIFFI_DIR)/libtezos.so
	@ln -f $(UNIFFI_DIR)/tezos.swift $(UNIFFI_BINDINGS_DIR)/tezos.swift
	@ln -f $(UNIFFI_DIR)/tezosFFI.h $(UNIFFI_BINDINGS_DIR)/tezosFFI.h
	@ln -f $(UNIFFI_DIR)/tezosFFI.modulemap $(UNIFFI_BINDINGS_DIR)/tezosFFI.modulemap
	@ln -f $(UNIFFI_DIR)/libtezos.so $(RESOURCES_DIR)/libtezos.so

# Build the `Tezos` lib from `tezos.swift`
.PHONY: build
build: link-lib \
	$(UNIFFI_BINDINGS_DIR)/tezos.swift \
	$(UNIFFI_BINDINGS_DIR)/tezosFFI.h \
	$(UNIFFI_BINDINGS_DIR)/tezosFFI.modulemap \
	$(RESOURCES_DIR)/libtezos.so
	@swiftc \
		-module-name Tezos \
		-emit-module-path $(LIB_DIR)/Tezos.swiftmodule \
		-emit-library -o $(RESOURCES_DIR)/libtezosswift.so \
		-Xcc -fmodule-map-file=$(UNIFFI_BINDINGS_DIR)/tezosFFI.modulemap \
		$(UNIFFI_BINDINGS_DIR)/tezos.swift


.PHONY: build-test
build-test: build $(BUILD_DIR) \
	$(UNIFFI_BINDINGS_DIR)/tezosFFI.h \
	$(UNIFFI_BINDINGS_DIR)/tezosFFI.modulemap \
	$(RESOURCES_DIR)/libtezos.so \
	$(RESOURCES_DIR)/libtezosswift.so \
	$(LIB_DIR)/Tezos.swiftmodule
	@swiftc \
		-I $(LIB_DIR) \
		-L $(RESOURCES_DIR) -ltezos -ltezosswift \
		-Xcc -fmodule-map-file=$(UNIFFI_BINDINGS_DIR)/tezosFFI.modulemap \
		-o $(TEST_EXE) \
		$(TEST_DIR)/*.swift


.PHONY: test
test: build-test \
	$(RESOURCES_DIR)/libtezos.so \
	$(RESOURCES_DIR)/libtezosswift.so \
	$(TEST_EXE)
	@LD_LIBRARY_PATH=$(LD_LIBRARY_PATH):$(RESOURCES_DIR) ./$(TEST_EXE)

.PHONY: clean
clean:
	@rm -f $(UNIFFI_BINDINGS_DIR)/tezos*
	@rm -f $(RESOURCES_DIR)/libtezos*
	@rm -f $(LIB_DIR)/Tezos*
	@rm -f $(TEST_EXE)
