#!/bin/sh

# We can't do linting on frozen protocol (src/proto_*/lib_protocol/*.ml) but we
# can still do it on the in-development protocol
# (src/proto_alpha/lib_protocol/*.ml). The `exclude` pattern below expresses
# this.
semgrep --metrics=off --error -l ocaml -c scripts/semgrep/ --exclude "src/proto_[0-9]*/lib_protocol/*.ml*" src/
