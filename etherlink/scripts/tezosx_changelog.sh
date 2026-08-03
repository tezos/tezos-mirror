#!/bin/sh
#
# SPDX-License-Identifier: MIT
# SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
#
# Manage the conflict-free changelog fragments of etherlink/CHANGES_TEZOSX.md.
#
# Instead of appending to the 'Unreleased' section of the changelog — which
# every merge request would then edit in the same place, causing rebase
# conflicts and entries stranded above a freshly cut release header — each
# merge request adds one file named after its merge request number in
# etherlink/.changes/tezosx/<section>/. Those fragments are assembled into
# CHANGES_TEZOSX.md once, when a release is cut.
#
# See etherlink/.changes/tezosx/README.md.

set -eu

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
ETHERLINK_DIR=$(dirname "$SCRIPT_DIR")
CHANGES_DIR="$ETHERLINK_DIR/.changes/tezosx"
CHANGELOG="$ETHERLINK_DIR/CHANGES_TEZOSX.md"

# Directory in which a merge request that needs no changelog entry says so, by
# adding an empty file named after its number.
NO_CHANGELOG_DIR="no_changelog"

# Sections of CHANGES_TEZOSX.md, as '<directory>|<heading>', in the order in
# which they appear in the changelog. This table is the single source of truth
# for the set of valid section directories.
sections() {
  cat << 'EOF'
evm_runtime|EVM Runtime
michelson_runtime|Michelson Runtime
nac|Native Atomic Composability
storage_versions|Storage versions
internals|Internals
EOF
}

# The body of the 'Unreleased' section, regenerated as-is by 'release'.
unreleased_block() {
  cat << 'EOF'
## Unreleased

Entries for the next release live as one file per merge request under
[`.changes/tezosx/`](.changes/tezosx/), to avoid rebase conflicts and entries
stranded above a freshly cut release header. See
[`.changes/tezosx/README.md`](.changes/tezosx/README.md).
EOF
}

error() {
  printf 'Error: %s\n' "$1" >&2
}

die() {
  error "$1"
  exit 1
}

usage() {
  cat << EOF
Usage: $0 <command> [<argument>...]

Commands:
  check [<mr-number>]        Check that the merge request declares a changelog
                             entry, or declares that it needs none. Defaults to
                             \$CI_MERGE_REQUEST_IID; only checks the repository
                             invariants if that variable is unset.
  preview                    Print the changelog section that 'release' would
                             generate from the fragments currently present.
  release <version> <hash>   Insert the assembled section as
                             '## Version <version> (<hash>)' in
                             etherlink/CHANGES_TEZOSX.md and delete the
                             consumed fragments.
EOF
}

# List the section directories.
section_dirs() {
  sections | cut -d'|' -f1
}

# List the directories in which a merge request can declare itself: one per
# changelog section, plus the one saying that no entry is needed.
declaration_dirs() {
  section_dirs
  printf '%s\n' "$NO_CHANGELOG_DIR"
}

# Report the layout problems of a single file of the fragment directory, if any.
# Kept in its own function because bash 3.2 (the /bin/sh of macOS) mis-parses a
# 'case' statement nested in a command substitution.
validate_file() {
  rel=${1#"$CHANGES_DIR"/}
  case "$rel" in
  README.md) return 0 ;;
  */*) ;;
  *)
    printf 'unexpected file %s\n' "$rel"
    return 0
    ;;
  esac
  dir=${rel%%/*}
  base=${rel#*/}
  if ! declaration_dirs | grep -qx -- "$dir"; then
    printf 'unknown section directory in %s\n' "$rel"
    return 0
  fi
  case "$base" in
  .gitkeep) return 0 ;;
  *.md) ;;
  *)
    printf 'fragment %s is not a markdown file\n' "$rel"
    return 0
    ;;
  esac
  case "${base%.md}" in
  '' | *[!0-9]*)
    printf 'fragment %s is not named <mr-number>.md\n' "$rel"
    return 0
    ;;
  esac
  if [ "$dir" = "$NO_CHANGELOG_DIR" ]; then
    if [ -s "$1" ]; then
      printf '%s must be empty: it only says that this merge request needs no changelog entry\n' "$rel"
    fi
    return 0
  fi
  if [ ! -s "$1" ]; then
    printf 'fragment %s is empty\n' "$rel"
  elif ! head -1 "$1" | grep -q '^- '; then
    printf 'fragment %s does not start with a markdown bullet ("- ")\n' "$rel"
  elif grep -q "(!${base%.md})" "$1"; then
    printf 'fragment %s spells out its own merge request reference, which is appended automatically\n' "$rel"
  fi
}

# Report the merge requests that both provide an entry and declare that they
# need none.
validate_no_contradiction() {
  for marker in "$CHANGES_DIR/$NO_CHANGELOG_DIR"/*.md; do
    [ -e "$marker" ] || continue
    mr=$(basename "$marker" .md)
    section_dirs | while read -r dir; do
      if [ -f "$CHANGES_DIR/$dir/$mr.md" ]; then
        printf '%s/%s.md says that no entry is needed, but %s/%s.md provides one\n' \
          "$NO_CHANGELOG_DIR" "$mr" "$dir" "$mr"
      fi
    done
  done
}

# Check that the fragment directory contains nothing but the README, the
# .gitkeep placeholders and files named '<mr-number>.md' in a known directory. A
# fragment in a misspelled section directory would otherwise be silently ignored
# when the release is assembled.
validate_layout() {
  errors=$(
    find "$CHANGES_DIR" -type f | sort | while read -r file; do
      validate_file "$file"
    done
    validate_no_contradiction
  )
  if [ -n "$errors" ]; then
    printf '%s\n' "$errors" | while read -r line; do error "$line"; done
    printf 'Valid directories are:\n' >&2
    declaration_dirs | sed 's|^|  .changes/tezosx/|' >&2
    return 1
  fi
}

# Check that nobody wrote an entry in the changelog itself: the 'Unreleased'
# section is written by 'release' only, and holds nothing but a pointer to the
# fragment directory.
check_unreleased_empty() {
  stranded=$(
    awk '
      /^## Unreleased/ { inside = 1; next }
      /^## / { inside = 0 }
      inside && /^(- |### )/ { print }
    ' "$CHANGELOG"
  )
  if [ -n "$stranded" ]; then
    error "the 'Unreleased' section of ${CHANGELOG#"$ETHERLINK_DIR"/} is not empty."
    cat >&2 << EOF
The Tezos X changelog is written by '$0 release' only. An entry belongs in a
fragment under .changes/tezosx/, not in the changelog itself — that is what
keeps it conflict-free. Move these lines to a fragment:

$stranded
EOF
    return 1
  fi
}

# List the '<mr-number>.md' files of a directory, most recent merge request
# first.
list_fragments() {
  dir="$CHANGES_DIR/$1"
  [ -d "$dir" ] || return 0
  {
    find "$dir" -maxdepth 1 -type f -name '*.md' |
      sed 's|.*/||; s|\.md$||' |
      grep -x '[0-9][0-9]*' |
      sort -rn |
      sed "s|^|$dir/|; s|\$|.md|"
  } || true
}

# Print the contents of a fragment, appending its merge request reference at the
# end of every bullet that does not already carry one.
print_fragment() {
  awk -v mr="$(basename "$1" .md)" '
    { lines[NR] = $0 }
    END {
      last = 0
      for (i = 1; i <= NR; i++) if (lines[i] ~ /[^ \t]/) last = i
      if (last == 0) exit 0
      for (i = 1; i <= last; i++) {
        if (lines[i] !~ /[^ \t]/) continue
        # A bullet ends on the last non-blank line of the fragment, and on the
        # line before the one that opens the next bullet.
        ends = (i == last)
        if (!ends) {
          for (j = i + 1; j <= last && lines[j] !~ /[^ \t]/; j++) continue
          ends = (lines[j] ~ /^- /)
        }
        if (ends && lines[i] !~ /\(![0-9]+\)[ \t]*$/) lines[i] = lines[i] " (!" mr ")"
      }
      for (i = 1; i <= last; i++) print lines[i]
    }
  ' "$1"
}

# Print the sections assembled from the fragments currently present. Sections
# without any fragment are omitted.
assemble() {
  sections | while IFS='|' read -r dir heading; do
    fragments=$(list_fragments "$dir")
    if [ -n "$fragments" ]; then
      printf '### %s\n\n' "$heading"
      printf '%s\n' "$fragments" | while read -r fragment; do
        print_fragment "$fragment"
      done
      printf '\n'
    fi
  done
}

# Print how a merge request declares itself, one path per line.
declarations_of() {
  declaration_dirs | while read -r dir; do
    if [ -f "$CHANGES_DIR/$dir/$1.md" ]; then
      printf '  .changes/tezosx/%s/%s.md\n' "$dir" "$1"
    fi
  done
}

# Explain how to declare the changelog entry of a merge request.
declaration_help() {
  cat << EOF
Add the entry of this merge request to the Tezos X changelog by creating

  etherlink/.changes/tezosx/<section>/$1.md

where <section> is one of:

$(section_dirs | sed 's|^|  |')

The file holds the markdown bullet(s) to insert under that section, for
instance:

  - The kernel no longer accepts blueprints signed by a rotated sequencer key.

Do not write the '(!$1)' reference: it is appended when the release is
assembled. See etherlink/.changes/tezosx/README.md.

If this merge request needs no changelog entry, say so with an empty file:

  touch etherlink/.changes/tezosx/$NO_CHANGELOG_DIR/$1.md
EOF
}

cmd_check() {
  validate_layout
  check_unreleased_empty
  mr=${1:-${CI_MERGE_REQUEST_IID:-}}
  if [ -z "$mr" ]; then
    printf 'No merge request number given: only checked the changelog invariants.\n'
    return 0
  fi
  found=$(declarations_of "$mr")
  if [ -n "$found" ]; then
    printf 'Merge request !%s declares its Tezos X changelog entry:\n%s\n' "$mr" "$found"
    return 0
  fi
  error "merge request !$mr declares no Tezos X changelog entry."
  printf '\n%s\n' "$(declaration_help "$mr")" >&2
  return 1
}

cmd_preview() {
  validate_layout
  body=$(assemble)
  if [ -z "$body" ]; then
    printf 'No changelog fragment in %s.\n' "${CHANGES_DIR#"$ETHERLINK_DIR"/}"
    return 0
  fi
  printf '%s\n' "$body"
}

cmd_release() {
  if [ $# -ne 2 ]; then
    usage >&2
    exit 1
  fi
  version=$1
  hash=$2
  case "$version" in
  '' | *[!0-9.]*) die "'$version' is not a valid version number (expected e.g. 0.8)." ;;
  esac
  case "$hash" in
  '' | *[!0-9a-f]*) die "'$hash' is not a valid kernel commit hash." ;;
  esac
  validate_layout
  # 'Unreleased' is rewritten below, so an entry left there would be lost.
  check_unreleased_empty
  body=$(assemble)
  if [ -z "$body" ]; then
    die "no changelog fragment in ${CHANGES_DIR#"$ETHERLINK_DIR"/}: nothing to release."
  fi
  tmp=$(mktemp)
  {
    awk '/^## Unreleased/ { exit } { print }' "$CHANGELOG"
    unreleased_block
    printf '\n## Version %s (%s)\n\n%s\n\n' "$version" "$hash" "$body"
    awk 'found || /^## Version / { found = 1; print }' "$CHANGELOG"
  } > "$tmp"
  mv "$tmp" "$CHANGELOG"
  count=0
  for dir in $(declaration_dirs); do
    for fragment in $(list_fragments "$dir"); do
      rm -f "$fragment"
      count=$((count + 1))
    done
  done
  printf 'Updated %s with version %s (%s).\n' \
    "${CHANGELOG#"$ETHERLINK_DIR"/}" "$version" "$hash"
  printf 'Removed %s changelog declaration(s). Review the result with:\n' "$count"
  printf '  git add -A etherlink && git diff --cached etherlink\n'
}

if [ $# -lt 1 ]; then
  usage >&2
  exit 1
fi
command=$1
shift
case "$command" in
check) cmd_check "$@" ;;
preview) cmd_preview "$@" ;;
release) cmd_release "$@" ;;
-h | --help | help) usage ;;
*)
  usage >&2
  exit 1
  ;;
esac
