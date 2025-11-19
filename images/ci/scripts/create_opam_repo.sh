#!/bin/sh

set -eu

# use opam proxy, if available
# shellcheck disable=SC2034
OPAMFETCH="/tmp/kiss-fetch.sh"
# shellcheck disable=SC2034
KISSCACHE="http://kisscache.kisscache.svc.cluster.local"

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

echo "Add package: release-tools-deps"
mkdir -p opam-repository/packages/release-tools-deps/release-tools-deps.dev
cp release-tools-deps.opam.locked opam-repository/packages/release-tools-deps/release-tools-deps.dev/opam

echo "Add packages: dream-httpaf, dream"
mkdir -p 'opam-repository/packages/dream-httpaf/dream-httpaf.1.0.0~alpha4-octez'
cp dream-httpaf.opam.locked 'opam-repository/packages/dream-httpaf/dream-httpaf.1.0.0~alpha4-octez/opam'
mkdir -p 'opam-repository/packages/dream/dream.1.0.0~alpha8-octez'
cp dream.opam.locked 'opam-repository/packages/dream/dream.1.0.0~alpha8-octez/opam'

echo "Add package: cohttp"
mkdir -p 'opam-repository/packages/cohttp/cohttp.5.3.1~octez'
cp cohttp.opam.locked 'opam-repository/packages/cohttp/cohttp.5.3.1~octez/opam'
mkdir -p 'opam-repository/packages/cohttp-lwt/cohttp-lwt.5.3.1~octez'
cp cohttp-lwt.opam.locked 'opam-repository/packages/cohttp-lwt/cohttp-lwt.5.3.1~octez/opam'
mkdir -p 'opam-repository/packages/cohttp-lwt-unix/cohttp-lwt-unix.5.3.1~octez'
cp cohttp-lwt-unix.opam.locked 'opam-repository/packages/cohttp-lwt-unix/cohttp-lwt-unix.5.3.1~octez/opam'

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
  "octez-deps,release-tools-deps,ocaml,ocaml-base-compiler,odoc,ledgerwallet-tezos,caqti-driver-postgresql,$dummy_pkg" \
  --environment "os=linux,arch=$arch,os-family=alpine"

# Clean up: remove packages that we do not actually want to install.
rm -rf packages/"$dummy_pkg" packages/octez-deps packages/release-tools-deps

# Retry mechanism for [opam admin add-hashes]
# The [opam admin add-hashes] command often hangs when fetching hashes from erratique.ch.
# To mitigate this, we implement a retry mechanism with exponential backoff.
# This approach retries the command up to 3 times with increasing delays between attempts.
# If the command fails, it waits for a specified delay before retrying.
# The delay increases exponentially to reduce the load on the server and give it time to recover.
MAX_RETRIES=3
# Initial delay, in seconds.
# Actual delays are 10, 30 and 90 seconds.
RETRY_DELAY=10

echo "Add safer hashes."
opam admin list --short | while read -r line; do
  for i in $(seq 1 $MAX_RETRIES); do
    if opam admin add-hashes sha256 sha512 -p "$line"; then
      break
    else
      echo "Failed to add hashes. Attempt $i of $MAX_RETRIES ❌"
      if [ "$i" -lt $MAX_RETRIES ]; then
        echo "Retrying in $RETRY_DELAY seconds..."
        sleep $RETRY_DELAY
        # Use exponential backoff
        RETRY_DELAY=$((RETRY_DELAY * 3))
      else
        echo "All attempts to add hashes failed. 🚨"
        exit 1
      fi
    fi
  done
done

cd ..
