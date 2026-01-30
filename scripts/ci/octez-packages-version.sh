#!/bin/sh

set -eu

# Set the logic for software releases
#
# We export the env var RELEASETYPE with the following values:
# - ReleaseCandidate : branch is protected, a tag is set, and there is an
#   associated valid release candidate version
# - Release : branch is protected, a tag is set, and there is an
#   associated valid release version
# - Beta : branch is protected, a tag is set, and there is an
#   associated beta version
# - Rebuild : branch is protected, a tag is set, and there is an
#   associated valid packaging revision
# - Master : branch is protected, a tag is not a release or not set
# - SoftRelease : branch is protected, a tag is set that is not a release
# - TestReleaseCandidate : branch is not protected, a tag is set, and there
#   is an associated valid release candidate version
# - TestRelease : A tag is set, Namespace is not tezos and there
#   is an associated valid release version
# - TestBeta : A tag is set, Namespace is not tezos and there
#   is an associated valid beta version
# - TestRebuild : A tag is set, Namespace is not tezos and there
#   is an associated valid packaging
# - TestBranch : branch is not protected
# - TestProtectedBranch : branch is protected

export RELEASETYPE=
export VERSION=

if [ "${CI_COMMIT_REF_NAME:-}" = "master" ]; then
  # master is protected by default.
  # protected, tag, master
  # Since it's master, we ignore the tag
  export RELEASETYPE="Master"
else
  # we might be interested in the tag even if the branch is not protected
  if [ -n "${CI_COMMIT_TAG:-}" ]; then
    # shellcheck source=./scripts/releases/octez-release.sh
    . ./scripts/releases/octez-release.sh
  fi

  if [ ! "${CI_COMMIT_REF_PROTECTED:-}" = "false" ]; then
    if [ -n "${CI_COMMIT_TAG:-}" ]; then
      if [ -n "${gitlab_release_no_v:-}" ]; then
        export VERSION="${gitlab_release_no_v:-}"
        if [ -n "${gitlab_release_rc_version:-}" ]; then
          # protected, tag, Release candidate
          if [ "${CI_PROJECT_NAMESPACE:-}" = "tezos" ]; then
            export RELEASETYPE="ReleaseCandidate"
          else
            export RELEASETYPE="TestReleaseCandidate"
          fi
        elif [ -n "${gitlab_release_beta_version:-}" ]; then
          # protected, tag, Beta
          if [ "${CI_PROJECT_NAMESPACE:-}" = "tezos" ]; then
            export RELEASETYPE="Beta"
          else
            export RELEASETYPE="TestBeta"
          fi
        elif [ -n "${gitlab_packaging_revision_version:-}" ]; then
          # protected, tag, Rebuild
          if [ "${CI_PROJECT_NAMESPACE:-}" = "tezos" ]; then
            export RELEASETYPE="Rebuild"
          else
            export RELEASETYPE="TestRebuild"
          fi
        else
          # protected, tag, Release
          if [ "${CI_PROJECT_NAMESPACE:-}" = "tezos" ]; then
            export RELEASETYPE="Release"
          else
            export RELEASETYPE="TestRelease"
          fi
        fi
      else
        # protected, tag
        # this is a tag on any protected branch that is not a release
        # Not a valid release
        export RELEASETYPE="SoftRelease"
      fi
    else
      # protected, !tag, other protected branches
      export RELEASETYPE="TestProtectedBranch"
    fi
  else
    # ! protected
    export RELEASETYPE="TestBranch"
    export VERSION="${gitlab_release_no_v:-}"
  fi
fi
