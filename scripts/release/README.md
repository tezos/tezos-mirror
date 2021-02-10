# Static Octez binaries automatic releasing

This directory contains scripts required for releasing static Octez
binaries:
* `binaries.sh` script creates a list of binaries that are going to be published
  this list is later used in other scripts.
* `upload-static-binaries-to-package-registry.sh` script uploads all the binaries
  to the gitlab package registry. Registry should be defined by the `PACKAGE_REGISTRY_URL`
  environment variable. Access to the package registry is provided by the token defined in
  the `CI_JOB_TOKEN` environment variable.
* `create-release-with-static-binaries.sh` script creates a release using the binaries that
  were previously uploaded to the package registry as release assets. Source package registry
  should be defined by the `PACKAGE_REGISTRY_URL` environment variable. Release name and tag
  are defined by the `CI_COMMIT_TAG` environment variable.
