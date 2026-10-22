#!/bin/sh

# SPDX-License-Identifier: MIT
# SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>

## This script is not meant to be executed interactively. Instead it is meant
## to be sourced to provide the constants that belong to the CI alone. For now
## that is just the version tag of the CI base images; the CI-only constants
## still sitting in scripts/version.sh belong here too.
##
## Like scripts/version.sh, it must stay compatible with both sh and make
## syntax (only `export var=value` lines and comments), as it may be sourced by
## shell scripts and included by makefiles.
##
## The CI generator ([ci/bin/main.exe]) reads [base_images_tag] from the
## environment (see [Images.Base_images] in [ci/lib_tezos_ci/tezos_ci.ml]), so
## this file is sourced by the CI build (see [ci/makefile]).

## Version tag shared by all base images (built together by the
## [base_images.daily] pipeline). Current version created by
## https://gitlab.com/tezos/tezos/-/pipelines/2791229003 (commit 18884dda).
export base_images_tag=master-18884dda
