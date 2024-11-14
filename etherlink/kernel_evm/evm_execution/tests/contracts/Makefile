# SPDX-FileCopyrightText: 2023 PK Lab <contact@pklab.io>
#
# SPDX-License-Identifier: MIT

.PHONY: artifacts

artifacts:
	mkdir artifacts || true
	forge build
	jq ".abi" build/MockFaBridgeWrapper.sol/MockFaBridgeWrapper.json > artifacts/MockFaBridgeWrapper.abi
	jq ".abi" build/MockFaBridgePrecompile.sol/MockFaBridgePrecompile.json > artifacts/MockFaBridgePrecompile.abi
	jq -r ".bytecode.object" build/MockFaBridgeWrapper.sol/MockFaBridgeWrapper.json | xxd -r -p > artifacts/MockFaBridgeWrapper.bytecode
