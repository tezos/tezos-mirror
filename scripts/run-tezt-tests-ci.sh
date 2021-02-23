#! /bin/sh

set -e

usage () {
    cat >&2 <<EOF
usage: [dry_run=true] $0 <from> [<to>]

This script allows the CI to run a subset of the Tezt tests (without
hard-to-maintain knowledge of all the tests).

It builds a temporary script containing a big shell command and then either
displays it when 'dry_run=true' or runs it.

Examples:

    dry_run=true scripts/run-tezt-tests-ci.sh 98

displays a command running the tests in the range [98; last].

    scripts/run-tezt-tests-ci.sh 5 9

runs the tests from the range [5; 9].
EOF
}

from_line="$1"
if [ "$from_line" = "" ] ; then
    usage
    exit 2
fi
to_line="$2"
if [ "$to_line" = "" ] ; then
    to_line=$(dune exec tezt/tests/main.exe -- --list-tsv | wc -l)
fi

to_run=$(mktemp)
echo "Using $to_run [ $from_line , $to_line ]" >&2

cat > "$to_run" <<EOF
dune exec tezt/tests/main.exe -- \\
     --color --log-buffer-size 5000 \\
     --log-file tezt.log --global-timeout 3300 --time \\
EOF
# This adds a line of the form '--test "<title>" \' for each test in the range:
dune exec tezt/tests/main.exe -- --list-tsv \
    | sed -n "$from_line,$to_line p" \
    | cut -f 2 \
    | sed "s/\(.*\)/--test \"\1\" \\\\/" >> "$to_run"
# the previous line ends with '\' so we add an empty variable to help the
# shell succeed in parsing the command:
echo '$extra_empty_arg' >> "$to_run"

if [ "$dry_run" = "true" ] ; then
    echo "Selected from $from_line to $to_line:"
    cat "$to_run"
else
    cat "$to_run"
    sh "$to_run"
fi
