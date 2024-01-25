#!/bin/sh

set -eu

# misc linting
find . ! -path "./_opam/*" -name "*.opam" -exec opam lint {} +

# Check that python environment is synchronized with the image's.
diff poetry.lock /home/tezos/poetry.lock
diff pyproject.toml /home/tezos/pyproject.toml

make check-linting

# python checks
make check-python-linting
make check-python-typecheck

# Ensure that all unit tests are restricted to their opam package
make lint-tests-pkg

# FIXME: https://gitlab.com/tezos/tezos/-/issues/2971
# The new version of odoc (2.1.0) is stricter than the old version (1.5.3),
# we temporarily deactivate the odoc checks.
## Ensure there are no mli docstring syntax errors in alpha protocol
#- ODOC_WARN_ERROR=true dune build @src/proto_alpha/lib_protocol/doc
# check that the hack-module patch applies cleanly
git apply devtools/protocol-print/add-hack-module.patch

# check that yes-wallet builds correctly
dune build devtools/yes_wallet/yes_wallet.exe

# check that the patch-yes_node.sh applies correctly
scripts/patch-yes_node.sh --dry-run
