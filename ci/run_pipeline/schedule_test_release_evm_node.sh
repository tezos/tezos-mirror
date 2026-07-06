#!/bin/sh

# Trigger a *test* release pipeline for the Octez EVM node (Etherlink) without
# pushing a tag, by creating a scheduled pipeline of kind
# 'etherlink.scheduled_test_release'. This is the tagless counterpart of the
# EVM node release, useful to test changes to the release pipeline (e.g. the
# 'ci-release' image) without publishing anything.
#
# Example: run it on your fork (nomadic-labs/tezos) against branch 'my-branch'.
# The branch must already be pushed to the fork.
#
#   git push origin my-branch
#   PROJECT_ID=9487506 \
#   CI_PROJECT_NAMESPACE=nomadic-labs \
#   BRANCH=my-branch \
#   ./ci/run_pipeline/schedule_test_release_evm_node.sh
#
# The script reads the GitLab API token from GITLAB_TOKEN, or prompts for it.
# Without any variable it defaults to the current branch on tezos/tezos
# (PROJECT_ID 3836952); the '--dry-run' publish step keeps it from publishing
# anything there.

SCRIPT_DIR="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
cd "$SCRIPT_DIR" || exit

TZ_SCHEDULE_KIND=etherlink.scheduled_test_release ./scheduled.sh
