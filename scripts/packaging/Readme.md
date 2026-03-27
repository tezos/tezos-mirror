# Debian packaging

The `octez` directory contains the Debian package specification for Octez.
These Debian packages are built using Debian tools and are going to replace the
existing Debian packages built by the scripts `scripts/dpkg/make_dpkg.sh`. We
call the packages in this directory the ``post-v19.1`` packages, and the
packages currently released the ``pre-19.1`` packages.

## Build the post-19.1 packages in the CI

The post-19.1 packages are binary packages, meaning that the build process is
not handled by `dpkg-buildpackage`, but the Octez binaries are instead given to
the Debian packaging tools. The binaries are built for each triple
distribution/release/architecture by calling `make` on the root of the Octez
project and then copied in the `scripts/packaging/binaries` directory to be
used by the Debian packaging tools.

The CI runs `scripts/ci/build-debian-packages.sh` inside a pre-built
`distro-build` Docker image to compile the binaries and create the Debian
packages.

#### Build dependencies

Build jobs use the pre-built `distro-build` Docker images defined in
`images/base-images/Dockerfile.debian-build` and published to the container
registry.

If you have never built Debian packages before you will
probably have to run `sudo apt install debhelper`.

## Build post-19.1 packages locally

To build the post-19.1 package on a developer machine, we need to recreate the
same environment used in the CI. The script
`scripts/packaging/build-deb-local.sh` compiles all the binary locally and
builds the Debian packages. The pre-requisite to this script is to run `make
build-deps` and ensure all runtime dependencies as well as the build
dependencies needed to build the Debian packages are correctly installed.

# Apt Repository Management

The file [package-signing-key.asc] contains the public key associated to
the gpg signing key stored in the CI. The CI / Release Manager will
sign the repository using this key, while the user can download
the public key to verify that the signature is indeed valid.

This user must download the key and make it known to apt.

Ex.

    curl "<apt-repository>/octez.asc" \
    | sudo gpg --dearmor -o /etc/apt/trusted.gpg.d/octez.gpg


### gutils

We use the gutils suite to upload the apt repository to the
google cloud storage. The key used to install those packages
is committed in the repository and can be refreshed from this url
`https://packages.cloud.google.com/apt/doc/apt-key.gpg`
