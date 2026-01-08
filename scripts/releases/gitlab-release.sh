#!/bin/sh
set -eu

# we run the smoketest only for x86_64 since this job is executed on x86_64
./scripts/ci/static_smoke_test.sh x86_64 version

# create a GitLab generic package
./scripts/ci/create_gitlab_package.sh

# create a GitLab release
./scripts/releases/create_gitlab_release.sh
