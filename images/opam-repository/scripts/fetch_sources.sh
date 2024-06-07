#!/bin/bash

set -e

readonly DEFAULT_EXCLUDED_PACKAGES=ocaml-option-

usage() {
  cat >&2 << EOF
usage: $0 <branch>+
       $0 --diff [<branch1>] <branch2>

Fetch sources of specified branches.

If --diff option is given, compare sources of branch1 (defaults to origin/master) and branch2.

The script must be run from the root of the opam repository.

Environment variables:
  Required:
    OPAMSWITCH
      Name of opam switch to use
    DIR
      Path to an existing directory to cache sources

  Optional:
    EXCLUDED_PACKAGES
      Comma-separated list of packages to exclude
      Default: $DEFAULT_EXCLUDED_PACKAGES

    DIFFTOOL
      Tool to compare directories
      Default: tries to use git difftool
EOF
}

declare COMMIT_HASH COMMIT_DIR SOURCES_DIR ALREADY_FETCHED_FILE REPO FRIENDLY_NAME package extra_branch GIT_ROOT DIFFTOOL DIFFTOOL_CMD DIFFTOOL_PATH
declare -a ALREADY_FETCHED PACKAGES package_with_version EXCLUDED SOURCES_DIRS

IFS=, read -ra EXCLUDED_PACKAGES <<< "${EXCLUDED_PACKAGES-$DEFAULT_EXCLUDED_PACKAGES}"

if [ -z "$OPAMSWITCH" ]; then
  echo "Variable OPAMSWITCH required"
  exit 1
fi
if ! opam switch list --short | grep --quiet --fixed-strings --line-regexp "$OPAMSWITCH"; then
  echo "Variable OPAMSWITCH does not point to a valid opam switch: $OPAMSWITCH"
  LATEST_SWITCH=$(opam switch list --short | head -n 1)
  readonly LATEST_SWITCH
  if [ -z "$LATEST_SWITCH" ]; then
    echo "No opam switch installed"
  else
    echo "Suggestion: OPAMSWITCH=\"$LATEST_SWITCH\""
  fi
  exit 1
fi

if [ -z "$DIR" ]; then
  echo "Variable DIR required"
  echo "Suggestion: DIR=\"$HOME/.tezos-opam-repository\""
  exit 1
fi
if ! [ -d "$DIR" ]; then
  echo "Variable DIR does not point to an existing directory: $DIR"
  echo "Suggestion: mkdir -p \"$DIR\""
  exit 1
fi

GIT_ROOT=$(git rev-parse --show-toplevel)
cd "$GIT_ROOT"

if ! [ -f "./repo" ]; then
  echo "The current directory does not look like an opam-repository"
  exit 1
fi

if [ $# -eq 0 ]; then
  usage
  exit 1
fi

if [ "$1" = "--diff" ]; then
  DO_DIFF=true
  shift
  if [ $# -gt 2 ]; then
    usage
    exit 1
  fi
  if [ $# -eq 1 ]; then extra_branch=origin/master; fi
  if [ -z "$DIFFTOOL" ]; then
    DIFFTOOL=$(git config --get diff.guitool || git config --get diff.tool || true)
    if [ -z "$DIFFTOOL" ]; then
      echo "Option --diff requires DIFFTOOL variable or git difftool configuration"
      echo "Available tools:"
      git difftool --tool-help | tail -n +2
      exit 1
    fi
  fi
  DIFFTOOL_PATH=$(git config --get "diff.$DIFFTOOL.path" 2> /dev/null || true)
  DIFFTOOL_CMD=$(git config --get "diff.$DIFFTOOL.cmd" 2> /dev/null || echo "$DIFFTOOL")
else
  DO_DIFF=false
fi

for REF_NAME in $extra_branch "$@"; do
  COMMIT_HASH=$(git rev-parse "$REF_NAME")
  if [ "$REF_NAME" = "$COMMIT_HASH" ]; then
    FRIENDLY_NAME="commit $REF_NAME"
  else
    FRIENDLY_NAME="branch $REF_NAME ($COMMIT_HASH)"
  fi
  COMMIT_DIR=$DIR/$COMMIT_HASH
  echo "  Copying opam repository from $FRIENDLY_NAME into $COMMIT_DIR..."
  if [ -f "$COMMIT_DIR/.cloned" ]; then
    echo "    Skipped because $COMMIT_DIR/ already exists."
  else
    mkdir -p "$COMMIT_DIR"
    GIT_WORK_TREE="$COMMIT_DIR" git restore --source="$COMMIT_HASH" --progress -- "*"
    touch "$COMMIT_DIR/.cloned"
    echo "    Done."
  fi
  echo "  Fetching sources..."
  SOURCES_DIR=$COMMIT_DIR/sources
  SOURCES_DIRS+=("$SOURCES_DIR")
  ALREADY_FETCHED_FILE=$COMMIT_DIR/.fetched_sources
  if [ -f "$ALREADY_FETCHED_FILE" ]; then
    mapfile -t ALREADY_FETCHED < "$ALREADY_FETCHED_FILE"
  else
    ALREADY_FETCHED=()
  fi
  if [ "${ALREADY_FETCHED[*]}" = "ALL" ]; then
    echo "    Skipped because all sources have been fetched in $SOURCES_DIR/."
  else
    if [ ${#ALREADY_FETCHED[@]} -gt 0 ]; then
      echo "    The following packages will be skipped because their sources have already been fetched:"
      printf "      %s\n" "${ALREADY_FETCHED[@]}"
    fi
    REPO=tezos-$COMMIT_HASH
    echo "    Adding repository $REPO to current opam switch..."
    if [ "$(opam repository list --short --this-switch | head -n 1)" = "$REPO" ]; then
      echo "      Skipped because $REPO is already the first-ranked repository for current switch."
    else
      opam repository add --this-switch "$REPO" "$COMMIT_DIR"
      echo "      Done."
    fi
    mkdir -p "$SOURCES_DIR"
    touch "$ALREADY_FETCHED_FILE"
    EXCLUDED=(-path .)
    for package in "${EXCLUDED_PACKAGES[@]}" "${ALREADY_FETCHED[@]}"; do
      EXCLUDED+=(-o -name "$package")
    done
    mapfile -t PACKAGES <<< "$(cd "$COMMIT_DIR/packages" && find . -maxdepth 1 \( "${EXCLUDED[@]}" \) -true -o -type d -printf "%f\n" | LC_COLLATE=C sort)"
    for package in "${PACKAGES[@]}"; do
      mapfile -t package_with_version <<< "$(cd "$COMMIT_DIR/packages/$package" && find . -mindepth 1 -maxdepth 1 -type d -printf "%f\n")"
      if [ ${#package_with_version[@]} -gt 1 ]; then
        echo "      For package $package, more than one version available. Please add it to EXCLUDED_PACKAGES list."
        exit 1
      fi
      echo "      Fetching sources for package ${package_with_version[0]}..."
      opam source --dir="$SOURCES_DIR/$package" "${package_with_version[0]}"
      echo "$package" >> "$ALREADY_FETCHED_FILE"
      echo "        Done."
      echo
    done
    echo "ALL" > "$ALREADY_FETCHED_FILE"
    echo "    Done fetching sources for all packages."
    echo "    Removing repository $REPO from current opam switch..."
    opam repository remove --all "$REPO"
    echo "      Done."
  fi
  echo
done

if $DO_DIFF; then
  echo "Diffing with $DIFFTOOL..."
  echo
  PATH="$DIFFTOOL_PATH:$PATH" "$DIFFTOOL_CMD" "${SOURCES_DIRS[@]}"
fi
