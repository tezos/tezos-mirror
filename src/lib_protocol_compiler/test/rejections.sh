#!/bin/sh

set -e

usage() {
  cat >&2 << EOF
usage: $0 <protocol-compiler-exec-path> <base-protocol>

This test creates erroneous protocols and checks that they are rejected by
the protocol compiler.

Example:

    bash src/lib_protocol_compiler/test/rejections.sh \\
         ./tezos-protocol-compiler \\
         src/proto_demo_noops/lib_protocol/TEZOS_PROTOCOL
EOF
}
say() {
  printf "[[compiler-rejections]]: %s\n" "$*" >&2
}

protocol_compiler="$1"
base_protocol="$2"

if ! [ -x "$protocol_compiler" ]; then
  say "erroneous protocol-compiler, should be an executable"
  usage
  exit 2
fi
if ! [ -f "$base_protocol" ]; then
  say "erroneous base-protocol"
  usage
  exit 2
fi

base_protocol_dirname=$(dirname "$base_protocol")

if ! [ -f "${base_protocol_dirname}/main.ml" ]; then
  say "wrong base-protocol: it needs to have a main.ml: ${base_protocol_dirname}/main.ml"
  usage
  exit 2
fi

try_compile() {
  tmp_out=$(mktemp -d /tmp/pcout-XXXXX)
  say "Trying $*"
  $protocol_compiler -no-hash-check -static -build-dir "$tmp_out" "$@"
}

should_fail() {
  say "$* SHOULD HAVE FAILED"
  exit 4
}

try_compile "$base_protocol_dirname"

################################################################################
#
# In `$with_external` we create a protocol that uses `external ..` to
# create an Obj.magic function.
#
# We put it deep in a local module to test the proper iteration of the AST.
#

with_external=$(mktemp -d /tmp/proto_007_XXXX)
cp "${base_protocol_dirname}/"* "$with_external/"

try_compile "$with_external/" # We check that we didn't break it with the `cp` :)

chmod u+w "$with_external/main.ml"
cat >> "$with_external/main.ml" << EOF
let _f =
  let module Hello = struct
    external hello : 'a -> 'b = "%identity"
  end in
  Hello.hello "bouh" + 42
EOF

{ try_compile "$with_external/" && should_fail "with-external"; } ||
  say "With external: 👍"

################################################################################
#
# In `$with_weird_ends` we create a protocol that tries to exploit a
# non-syntactically valid file to escape the functor and make the node
# run an infinite loop while dynamically loading it.
#

with_weird_ends=$(mktemp -d /tmp/proto_008_XXXX)
cp "${base_protocol_dirname}/"* "$with_weird_ends/"
try_compile "$with_weird_ends/" # We check that we didn't break it with the `cp` :)

cat > "$with_weird_ends/weird.ml" << EOF
end
include Main
end
let _ = while true do () done
module A = struct
module B = struct
  let _x = 51
EOF
cat > "$with_weird_ends/weird.mli" << EOF
(* nothing here *)
EOF
cat > "$with_weird_ends/closing.ml" << EOF
let x = 42
EOF
chmod u+w "$with_weird_ends/TEZOS_PROTOCOL"
sed 's/"Main"/"Main", "Weird", "Closing"/' "$base_protocol" > "$with_weird_ends/TEZOS_PROTOCOL"

{ try_compile "$with_weird_ends/" && should_fail "with-weird-ends"; } ||
  say "With weird ends: 👍"
