# SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
# SPDX-FileCopyrightText: 2023-2024 TriliTech <contact@trili.tech>
# SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
#
# SPDX-License-Identifier: MIT

.PHONY: all
all: check test

.PHONY: check
check:
	cargo fmt --check
	cargo check --all-targets
	# TODO: RV-339 - enable clippy; which will require code changes to pass due to version bump
	#cargo clippy --all-targets

.PHONY: test
test:
	cargo test
   
.PHONY: clean
clean:
	cargo clean
