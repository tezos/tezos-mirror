#!/bin/sh

set -eu

# Compute coverage
make coverage-report
# Rewrite the summary output to remove information points matching the coverage regexp below
make coverage-report-summary | sed 's@Coverage: [[:digit:]]\+/[[:digit:]]\+ (\(.*%\))@Coverage: \1@'
make coverage-report-cobertura
