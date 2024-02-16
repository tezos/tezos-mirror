# SPDX-FileCopyrightText: 2023 PK Lab <contact@pklab.io>
#
# SPDX-License-Identifier: MIT

.PHONY: artifacts

artifacts:
	mkdir artifacts || true
	forge build
	jq ".abi" build/MockWrapper.sol/MockWrapper.json > artifacts/MockWrapper.abi
	jq -r ".bytecode.object" build/MockWrapper.sol/MockWrapper.json | xxd -r -p > artifacts/MockWrapper.bytecode
	jq ".abi" build/MockPrecompile.sol/MockPrecompile.json > artifacts/MockPrecompile.abi
