#!/usr/bin/env bash
# Install cue workflow hooks.
#
# Git hooks go into the main repository's .git/hooks directory.
# Existing hooks there are backed up with a .bak suffix.
#
# Cue hooks go into .cue-store/.cue-hooks/ and are committed to the
# cue store's git repository.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

# Always resolve the repository root from SCRIPT_DIR
# (contrib/workflow/hooks/), not from the caller's cwd. This avoids
# installing git hooks into .cue-store/.git/hooks/ when install.sh is
# invoked from there.
REPO_ROOT="$(cd "$SCRIPT_DIR/../../.." && pwd)"

if [[ ! -d "$REPO_ROOT/.git" ]]; then
  echo "Error: $REPO_ROOT is not a git repository" >&2
  exit 1
fi

GIT_HOOKS_DIR="$REPO_ROOT/.git/hooks"
mkdir -p "$GIT_HOOKS_DIR"

for hook in pre-commit commit-msg; do
  source="$SCRIPT_DIR/$hook"
  target="$GIT_HOOKS_DIR/$hook"
  if [[ -e "$target" ]] && ! cmp -s "$source" "$target"; then
    echo "Backing up existing $hook to $hook.bak"
    mv "$target" "$target.bak"
  fi
  cp "$source" "$target"
  chmod +x "$target"
  echo "Installed git hook $hook"
done

CUE_HOOKS_DIR="$REPO_ROOT/.cue-store/.cue-hooks"
mkdir -p "$CUE_HOOKS_DIR"

for hook in pre-set post-add post-set pre-edit; do
  target="$CUE_HOOKS_DIR/$hook"
  cp "$SCRIPT_DIR/$hook" "$target"
  chmod +x "$target"
  echo "Installed cue hook $hook"
done

# Commit the installed cue hooks to the cue store's git repository.
cue git add .cue-hooks
cue git commit -m "installing cue hooks"
