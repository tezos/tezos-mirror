#! /usr/bin/env bash

script_dir=$(cd "$(dirname "$0")" && pwd -P)
src_dir="$(dirname "$script_dir")"

#shellcheck source=version.sh
. "$script_dir"/version.sh

readonly BISECT_FILE=$src_dir/$COVERAGE_OUTPUT/
readonly DIRECTIVE_PREFIX="(preprocess (pps bisect_ppx"
readonly LINE_TO_ADD="$DIRECTIVE_PREFIX -- --bisect-file $BISECT_FILE)) ; Added by $0"

usage () {
    cat >&2 <<EOF
usage: $0 [--add | --remove] <dune file / dir>+ [--except <dune file / dir>+]

This script adds the line
  $LINE_TO_ADD
to the library or executable stanzas of all Dune files passed
as parameters. It enables code covering instrumentation using
bisect_ppx.

--remove will remove lines starting with
  $DIRECTIVE_PREFIX
from the files.

--except do not touch specified files
EOF
}

if [ $# -eq 0 ]; then
    usage
    exit 1
fi

ACTION=add
REMOVE=false
if [ "$1" = "--add" ]; then
    shift
elif [ "$1" = "--remove" ]; then
    ACTION=remove
    REMOVE=true
    shift
fi

DUNES=()

while [ $# -gt 0 ]; do
    if [ "$1" = "--except" ]; then
        shift
        break 2
    else
        DUNES+=("$1")
    fi
    shift
done

PRUNE=(-name "_*")

while [ $# -gt 0 ]; do
    PRUNE+=(-o -path "$1" -o -path "./$1")
    shift
done

for dune in "${DUNES[@]}"; do

    if [ -d "$dune" ]; then
        DUNE_FILES=$(find "$dune" \( "${PRUNE[@]}" \) -prune -o -type f \( -name "dune" -o -name "*dune.inc" -o -name "dune_protocol.template*" \) -print)
    else
        DUNE_FILES=$dune
    fi

    for dune_file in $DUNE_FILES; do
        if [ -L "$dune_file" ]; then
            echo "${dune_file}: symbolic link. Skipping." >&2
            continue
        fi
        if grep -Fq "$DIRECTIVE_PREFIX" "$dune_file"; then
            if ! $REMOVE; then
                echo "${dune_file}: directive seems already present in file. Skipping." >&2
                continue
            fi
        elif $REMOVE; then
            echo "${dune_file}: directive absent from file. Skipping." >&2
            continue
        fi

        tmp=$(mktemp)

        if $REMOVE; then
            grep -vF "$DIRECTIVE_PREFIX" "$dune_file" > "$tmp"
        else
            # See this question for the Awk script
            # https://stackoverflow.com/questions/9970124/sed-to-insert-on-first-match-only
            awk "/public_name/ && !x {print \"$LINE_TO_ADD\"; x=0} 1" "$dune_file" > "$tmp"
        fi
        if [ "$?" -ne 0 ]; then
            echo "${dune_file}: unable to $ACTION directive to file. Skipping." >&2
            rm "$tmp"
        else
            lines_before=$(wc -l < "$dune_file")
            lines_after=$(wc -l < "$tmp")
            mv "$tmp" "$dune_file"
            if $REMOVE; then
                echo "${dune_file}: removed $((lines_before-lines_after)) time(s)."
            else
                echo "${dune_file}: added $((lines_after-lines_before)) time(s)."
            fi
        fi
    done
done
