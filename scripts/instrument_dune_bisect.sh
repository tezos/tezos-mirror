#!/bin/sh

readonly DIRECTIVE="(preprocess (pps bisect_ppx))"
readonly LINE_TO_ADD="$DIRECTIVE ; Added by $0"

usage () {
    cat >&2 <<EOF
usage: $0 [FILES]

This script adds the line
  $LINE_TO_ADD
to the library or executable stanzas of all Dune files passed
as parameters. It enables code covering instrumentation using
bisect_ppx.
EOF
}

if [ $# -eq 0 ]; then
    usage
    exit 1
fi

DUNE_FILES=$@

for dune_file in $DUNE_FILES; do
    if grep -Fq "$DIRECTIVE" $dune_file; then
        echo "${dune_file}: directive already present in file. Skipping." >&2
        continue
    fi

    tmp=$(mktemp)

    # See this question for the Awk script
    # https://stackoverflow.com/questions/9970124/sed-to-insert-on-first-match-only
    awk "/public_name/ && !x {print \"$LINE_TO_ADD\"; x=0} 1" $dune_file > $tmp

    if [ "$?" -ne 0 ]; then
        echo "${dune_file}: unable to add directive to file. Skipping." >&2
        rm $tmp
    else
        lines_before=`cat $dune_file | wc -l`
        lines_after=`cat $tmp | wc -l`
        lines_added=$((lines_after - lines_before))
        mv $tmp $dune_file
        echo "${dune_file}: added $lines_added time(s)."
    fi
done

