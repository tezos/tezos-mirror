#!/bin/sh

# test the version assiciated to a git tag. Here we use
# a random version and we check if it is correctly parsed
# The script src/lib_version/tezos_print_version.exe prints the
# same version displayed by tezos-node --version

set -e

VERSION=10.94
RANDOMTAG="testtesttest"
TESTBRANCH=$RANDOMTAG
CURRENT_BRANCH=$(git rev-parse --abbrev-ref HEAD)

cleanup () {
  set +e
  git tag -d "$RANDOMTAG" > /dev/null 2>&1
  git tag -d "$VERSION" > /dev/null 2>&1
  git tag -d "v$VERSION" > /dev/null 2>&1
  git tag -d "v$VERSION"+rc1 > /dev/null 2>&1
  git tag -d "v$VERSION"-rc1 > /dev/null 2>&1
  git checkout "$CURRENT_BRANCH"
  git branch -D $TESTBRANCH || true
}
trap cleanup EXIT INT

cleanup

git checkout -b $TESTBRANCH

rm -f _build/default/src/lib_version/generated_git_info.ml
res=$(dune exec -f src/lib_version/tezos_print_version.exe || true)
test -z "$res" && echo "Last tag on the branch $res"

git tag "$VERSION" -m "test"
rm -f _build/default/src/lib_version/generated_git_info.ml
res=$(dune exec -f src/lib_version/tezos_print_version.exe || true)
echo "Last tag on the branch $res"
if [ "$res" != "$VERSION" ]; then
  echo "expected $VERSION; got $res : FAIL"
  exit 1
else
  echo "Tag: $VERSION ; Expected Version : $res : PASS"
fi

git tag "v$VERSION" -m "test"
rm -f _build/default/src/lib_version/generated_git_info.ml
res=$(dune exec -f src/lib_version/tezos_print_version.exe || true)
echo "Last tag on the branch $res"
if [ "$res" != "$VERSION" ]; then
  echo "expected $VERSION; got $res : FAIL"
  exit 1
else
  echo "Tag: v$VERSION ; Expected Version : $res : PASS"
fi

git commit --allow-empty -m "test" > /dev/null 2>&1
rm -f _build/default/src/lib_version/generated_git_info.ml
res=$(dune exec -f src/lib_version/tezos_print_version.exe || true)
if [ "$res" != "$VERSION+dev" ]; then
  echo "expected $VERSION+dev; got $res : FAIL"
  exit 1
else
  tag=$(git describe --tags)
  echo "Tag: $tag ; Expected Version : $res : PASS"
fi

git tag "$RANDOMTAG" -m "test"
rm -f _build/default/src/lib_version/generated_git_info.ml
res=$(dune exec -f src/lib_version/tezos_print_version.exe || true)
if [ "$res" != "0.0+dev" ]; then
  echo "expected 0.0+dev; got $res : FAIL"
  exit 1
else
  echo "Tag: $RANDOMTAG ; Expected Version : $res : PASS"
fi

git tag "v$VERSION"+rc1 -m "test"
rm -f _build/default/src/lib_version/generated_git_info.ml
res=$(dune exec -f src/lib_version/tezos_print_version.exe || true)
if [ "$res" != "$VERSION+dev" ]; then
  echo "expected $VERSION+dev; got $res : FAIL"
  exit 1
else
  echo "Tag: v$VERSION+rc1 ; Expected Version : $res : PASS"
fi

git tag "v$VERSION"-rc1 -m "test"
rm -f _build/default/src/lib_version/generated_git_info.ml
res=$(dune exec -f src/lib_version/tezos_print_version.exe || true)
if [ "$res" != "$VERSION~rc1" ]; then
  echo "expected $VERSION~rc1; got $res : FAIL"
  exit 1
else
  echo "Tag: v$VERSION-rc1 ; Expected Version : $res : PASS"
fi

git commit --allow-empty -m "test" > /dev/null 2>&1
rm -f _build/default/src/lib_version/generated_git_info.ml
res=$(dune exec -f src/lib_version/tezos_print_version.exe || true)
if [ "$res" != "$VERSION~rc1+dev" ]; then
  echo "expected $VERSION~rc1+dev; got $res : FAIL"
  exit 1
else
  tag=$(git describe --tags)
  echo "Tag: $tag ; Expected Version : $res : PASS"
fi

git checkout -

cleanup
