#!/usr/bin/env bash

set -e

# Checks if running in dry-mode
for arg in "$@"; do
  case $arg in
  "--dry-run")
    dry_run="--dry-run"
    echo "Running in dry-run mode. The opam packages won't be actually released."
    ;;
  esac
done

ci_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
script_dir="$(dirname "$ci_dir")"

opam_repository_fork="git@github.com:tezos/opam-repository"
opam_dir="opam-repository"

log() {
  printf '\e[1m%s\e[0m' "$1"
}

# shellcheck source=./scripts/releases/octez-release.sh
. "${script_dir}/releases/octez-release.sh"

# set up ssh credentials to access github
mkdir -p "$HOME/.ssh"
cp "$TEZOS_GITHUB_OPAM_REPOSITORY_MACHINE_USER_PRIVATE_SSH_KEY" "$HOME/.ssh/id_rsa"
cat "$GITHUB_SSH_HOST_KEYS" >> "$HOME/.ssh/known_hosts"
chmod 600 "$HOME/.ssh/known_hosts"
chmod 600 "$HOME/.ssh/id_rsa"
chmod 700 "$HOME/.ssh"
log "Done setting up credentials."

# call opam-release.sh with the correct arguments
echo "$script_dir/releases/opam-release.sh" \
  "$opam_release_tag" \
  "https://gitlab.com/tezos/tezos/-/archive/$CI_COMMIT_TAG/$gitlab_octez_source_package_name.tar.gz" \
  "$opam_dir" \
  "$dry_run"

"$script_dir/releases/opam-release.sh" \
  "$opam_release_tag" \
  "https://gitlab.com/tezos/tezos/-/archive/$CI_COMMIT_TAG/$gitlab_octez_source_package_name.tar.gz" \
  "$opam_dir" \
  "$dry_run"

# Matches the corresponding variable in /scripts/opam-release.sh.
branch_name="octez-$(echo "$opam_release_tag" | tr '~' -)"
latest_branch="octez-latest"

if [ -z "$dry_run" ]; then

  log "While we're here, update master on the fork..."
  cd "$opam_dir"
  git remote add github "$opam_repository_fork"
  git push github master:master

  log "Pushing $branch_name to $opam_repository_fork..."
  git push --force-with-lease github "${branch_name}:${branch_name}"

  log "Pushing $latest_branch to $opam_repository_fork..."
  git push github "${latest_branch}:${latest_branch}"

fi
