#!/bin/sh

set -e

original_pwd="$(pwd)"
script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"

usage="Usage: $0 <VERSION_NUMBER> <TARBALL_URL> [OPAM_REPOSITORY_CLONE_DIR]

Example: $0 11.0 https://gitlab.com/tezos/tezos/-/archive/v11.0/tezos-v11.0.tar.bz2

This script clones ocaml/opam-repository into OPAM_REPOSITORY_CLONE_DIR
(or uses the existing clone if it already exists) and generates opam packages
in a new branch named octez-<VERSION_NUMBER> in it. This branch should be
ready to be made into a pull request.

Default value for OPAM_REPOSITORY_CLONE_DIR is 'opam-repository'.

TARBALL_URL is the URL to put in opam files.
The script downloads it to compute sha256 and sha512 checksums for you."

version="$1"
url="$2"
opam_dir="$3"

if [ -z "$version" ] ; then
    echo "$usage"
    exit 1
fi

if [ -z "$url" ] ; then
    echo "$usage"
    exit 1
fi

if [ -z "$opam_dir" ] ; then
    opam_dir="opam-repository"
fi

log () {
    echo '\e[1m'"$1"'\e[0m'
}

if [ -d "$opam_dir" ] ; then
    log "Checking $opam_dir..."
    cd "$opam_dir"
    status=$(git status --porcelain)
    if [ -z "$status" ] ; then
        git checkout master
        git pull
    else
        log "$opam_dir is not clean."
        exit 1
    fi
else
    log "Cloning ocaml/opam-repository..."
    git clone https://github.com/ocaml/opam-repository "$opam_dir"
    cd "$opam_dir"
    git checkout master
fi

branch_name="octez-$version"
if git rev-parse "$branch_name" > /dev/null 2> /dev/null ; then
    log "Error: a branch named $branch_name already exists in $opam_dir."
    exit 1
fi

tarball=$(mktemp tezos_tarball.XXXXXXXX --tmpdir)

clean_tarball() {
    log "Cleaning up..."
    rm -f "$tarball"
}
trap clean_tarball EXIT

log "Downloading tarball from $url..."
curl "$url" --output "$tarball"

log "Hashing tarball..."
sha256=$(sha256sum "$tarball" | cut -d ' ' -f 1)
log "SHA256: $sha256"
sha512=$(sha512sum "$tarball" | cut -d ' ' -f 1)
log "SHA512: $sha512"

log "Generating opam files for release..."
cd "$script_dir"/../manifest
make manifest
./manifest \
    --packages-dir "$opam_dir/packages" \
    --url "$url" \
    --sha256 "$sha256" \
    --sha512 "$sha512" \
    --release "$version"

log "Creating commit..."
cd "$original_pwd"
cd "$opam_dir"
branch="octez-""$(echo "$version" | tr '~' -)"
git checkout -b "$branch"
git add packages
git commit -am "Octez $version packages"
log "A branch named $branch has been created in $opam_dir."
