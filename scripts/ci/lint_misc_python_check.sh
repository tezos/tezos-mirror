#!/bin/sh

set -eux

# Check that python environment is synchronized with the image's.
diff poetry.lock /home/tezos/poetry.lock
diff pyproject.toml /home/tezos/pyproject.toml

# python checks
make check-python-linting
make check-python-typecheck
