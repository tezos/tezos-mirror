#!/bin/sh

set -eux

# misc linting
make check-opam-linting

scripts/lint.sh --check-scripts
scripts/lint.sh --check-rust-toolchain

# Mempool in lib_plugin is usually removed when the associated protocol is frozen.
# However to replay Ghostnet's history, it is necessary to keep a fix in the
# mempool's validation.
scripts/lint.sh --check-quebec-plugin-ghostnet-fix

# Ensure that all unit tests are restricted to their opam package
make lint-tests-pkg

# FIXME: https://gitlab.com/tezos/tezos/-/issues/2971
# The new version of odoc (2.1.0) is stricter than the old version (1.5.3),
# we temporarily deactivate the odoc checks.
## Ensure there are no mli docstring syntax errors in alpha protocol
#- ODOC_WARN_ERROR=true dune build @src/proto_alpha/lib_protocol/doc
# check that the hack-module patch applies cleanly
git apply devtools/protocol-print/add-hack-module.patch

# Check that the protocol profiling patches can still be applied
scripts/patch-profiler-proto.sh --dry-run
