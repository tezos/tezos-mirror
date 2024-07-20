#!/bin/sh

set -eu

# When running the script manually, version.sh is in: scripts/
# But in the Dockerfile, the script is copied to: ./
if [ -e scripts/version.sh ]; then
  . scripts/version.sh
else
  # shellcheck source=scripts/version.sh
  . version.sh
fi

echo "Shallow clone of opam repository (requires git protocol version 2)"
mkdir opam-repository
cd opam-repository
git init
git config --local protocol.version 2
git remote add origin https://github.com/ocaml/opam-repository
git fetch --depth 1 origin "$opam_repository_tag"
git checkout "$opam_repository_tag"
# No need to store the whole Git history in the Docker images.
rm -rf .git .github .gitignore .gitattributes
cd ..

echo "Add package: octez-deps"
mkdir -p opam-repository/packages/octez-deps/octez-deps.dev
cp octez-deps.opam.locked opam-repository/packages/octez-deps/octez-deps.dev/opam

echo "Add package: dummy-tezos"
# This package adds some constraints to the solution found by the opam solver.
dummy_pkg=octez-dummy
dummy_opam_dir="opam-repository/packages/${dummy_pkg}/${dummy_pkg}.dev"
dummy_opam="${dummy_opam_dir}/opam"
mkdir -p "${dummy_opam_dir}"
echo 'opam-version: "2.0"' > "$dummy_opam"
echo "depends: [ \"ocaml\" { = \"$ocaml_version\" } ]" >> "$dummy_opam"
echo 'conflicts:[' >> "$dummy_opam"
grep -r "^flags: *\[ *avoid-version *\]" -l opam-repository/packages |
  LC_COLLATE=C sort -u |
  while read -r f; do
    f=$(dirname "$f")
    f=$(basename "$f")
    p=$(echo "$f" | cut -d '.' -f '1')
    v=$(echo "$f" | cut -d '.' -f '2-')
    echo "\"$p\" {= \"$v\"}" >> $dummy_opam
  done
# FIXME: https://gitlab.com/tezos/tezos/-/issues/5832
# opam unintentionally picks up a windows dependency. We add a
# conflict here to work around it.
echo '"ocamlbuild" {= "0.14.2+win" }' >> $dummy_opam
echo ']' >> "$dummy_opam"

# Display the file for debugging.
echo "Contents of $dummy_opam:"
cat "$dummy_opam"

echo "Remove all packages which are not needed by the packages we actually want."
# - octez-deps, i.e. the set of transitive dependencies of Octez
# - $dummy_pkg, our additional constraints
# - ocaml-base-compiler has to be explicitely listed for the solver
#   to not prefer the "variant" `system` of the compiler
# - odoc is used by the CI to generate the doc.
#   TODO: https://gitlab.com/tezos/tezos/-/issues/7085
#   remove restriction to odoc 2.2.0 once the issue above is
#   resolved.
# - ledgerwallet-tezos is an optional dependency of signer-services
#   we want to have when building released binaries
# - caqti-driver-postgresq is needed by tps measurement software to
#   read tezos-indexer databases

case "$(uname -m)" in
  "x86_64")
    arch="x86_64"
    ;;
  "aarch64")
    arch="arm64"
    ;;
  *)
    arch="unknown"
    ;;
esac

cd opam-repository
OPAMSOLVERTIMEOUT=600 opam admin filter --yes --resolve \
  "octez-deps,ocaml,ocaml-base-compiler,odoc<2.3.0,ledgerwallet-tezos,caqti-driver-postgresql,$dummy_pkg" \
  --environment "os=linux,arch=$arch,os-family=alpine"

# Clean up: remove packages that we do not actually want to install.
rm -rf packages/"$dummy_pkg" packages/octez-deps

echo "Add safer hashes."
opam admin add-hashes sha256 sha512
cd ..
