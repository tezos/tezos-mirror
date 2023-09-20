#!/bin/bash

#############################################################################
#                                                                           #
# Open Source License                                                       #
# Copyright (c) 2022-2023 Nomadic Labs <contact@nomadic-labs.com>           #
#                                                                           #
# Permission is hereby granted, free of charge, to any person obtaining a   #
# copy of this software and associated documentation files (the "Software"),#
# to deal in the Software without restriction, including without limitation #
# the rights to use, copy, modify, merge, publish, distribute, sublicense,  #
# and/or sell copies of the Software, and to permit persons to whom the     #
# Software is furnished to do so, subject to the following conditions:      #
#                                                                           #
# The above copyright notice and this permission notice shall be included   #
# in all copies or substantial portions of the Software.                    #
#                                                                           #
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR#
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  #
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   #
# THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER#
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   #
# FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       #
# DEALINGS IN THE SOFTWARE.                                                 #
#                                                                           #
#############################################################################

# This script detects if the S3 bucket storing CSVs resulting from snoop
# inference has been updated since the previous run of this script.

# If the bucket has not been updated since the previous run of this script then
# the script does nothing. So it is safe to run this script quite often in a
# Cron job.

# If the bucket contains new results, then the script compares each CSV in the
# directory of the last run with CSVs of the same name in all other directories.
# Old directories can be filtered out by setting the FIRST_DIR environment
# variable, which defaults to the oldest directory.

# The results of the last run are also compared to two reference points:
# * a reference run (declared in the REF_DIR environment variable; defaulting to
#   FIRST_DIR if the variable is empty or not set);
# * and the previous run (as read in the last_known_dir file).

# The comparison is done using the gas_parameter_diff OCaml script from the
# Tezos codebase.

# If regressions are detected during the diffs, they are sent to a Slack
# channel.

# The results of all comparisons are in up to 4 CSV tables:
# - all.csv comparing all the runs since FIRST_DIR (included)
# - reference.csv comparing the reference and last runs
# - previous.csv comparing the previous and last runs.
# - selected.csv that filters all.csv with the parameters on which a
#   regression was found.
# These tables are also sent to the Slack channel

# This is the Slack channel messages will be sent to (gas-benchmarks-reports).
CHAN=C04HZHR11DW
# This is the confidential Slack authorization token. It allows us to send
# messages as the gas-benchmarks-reports bot.
TOK="$(cat "$HOME"/slack_token)"

# To test this script without polluting the Slack channel, it is possible to
# deactivate Slack messaging by setting the environment variable
# DISABLE_SLACK_MESSAGING to any non-empty string.

slack() {
    if [ "$DISABLE_SLACK_MESSAGING" = "" ]
    then
        curl -X POST -H 'Authorization: Bearer '"$TOK" -H 'Content-type: application/json; charset=utf-8' --data "{\"channel\":\"$CHAN\",\"text\":\"$1\"}" https://tezos-dev.slack.com/api/chat.postMessage
    else
        echo "Message \"$1\" would be sent on Slack but Slack messaging has been disabled."
    fi
}

slack_send_file() {
    if [ "$DISABLE_SLACK_MESSAGING" = "" ]
    then
        curl -F file=@"$1" -F "initial_comment=$2" -F channels="$CHAN" -F token="$TOK" https://tezos-dev.slack.com/api/files.upload
    else
        echo "File \"$1\" would be sent on Slack with message \"$2\" but Slack messaging has been disabled."
    fi
}

OCTEZ_DIR="/data/redbull/tezos"

# Check if a directory more recent than the last known one exists on the bucket.
# If not, we have nothing to do so we stop immediately.
LAST_KNOWN_DIR="$(cat "$OCTEZ_DIR"/last_known_dir)"
echo "Last known dir: $LAST_KNOWN_DIR"

LAST_DIR="$(aws s3 ls s3://snoop-playground/mclaren/inference_csvs/snoop_results/ | grep PRE | tail -n 1 | sed 's/ *PRE //' | sed 's,/$,,')"
echo "Last dir: $LAST_DIR"

if [ "$LAST_DIR" = "$LAST_KNOWN_DIR" ]
then
    echo "No new results to analyse, exiting"
    exit 0
fi

# Check that the SUCCESS file is present. This file is the very last uploaded by
# the reference machine on successful runs. If the new directory is on the
# bucket but the SUCCESS file is not, it means that the upload is in progress so
# we stop immediately to avoid analysing incomplete data and we don't update the
# `last_known_dir` file so that the next run of this script can retry.
if [ "$(aws s3 ls s3://snoop-playground/mclaren/complete_results/snoop_results/"$LAST_DIR"/SUCCESS | wc -l)" = 0 ]
then
    echo "SUCCESS file not found. The benchmarks failed, or the benchmark machine has not finished uploading. Exiting"
    exit 0
fi

# We update the file content as soon as possible so that concurrent runs of this
# script are unlikely to step on each other's feet.
echo "$LAST_DIR" > "$OCTEZ_DIR"/last_known_dir

slack "New results have been uploaded to the S3 bucket in directory \`$LAST_DIR\`. I will look for regressions."

INPUT_CSV_DIR="$OCTEZ_DIR/input_csvs"
OUTPUT_CSV_DIR="$OCTEZ_DIR/output_csvs"

rm -rf "$INPUT_CSV_DIR"
mkdir -p "$INPUT_CSV_DIR"
aws s3 sync s3://snoop-playground/mclaren/inference_csvs/snoop_results/ "$INPUT_CSV_DIR"/

rm -rf "$OUTPUT_CSV_DIR"
mkdir -p "$OUTPUT_CSV_DIR"

ALERT_FILE="$OUTPUT_CSV_DIR/alerts"
SELECTION_FILE="$OUTPUT_CSV_DIR/selected.csv"

rm -f "$ALERT_FILE"
rm -f "$SELECTION_FILE"

# All the directories before FIRST_DIR will be ignored.
export FIRST_DIR
if [ "$FIRST_DIR" = "" ]
then
    # -maxdepth 1 not to apply the find recursively.
    # -mindepth 1 not to output the base directory $INPUT_CSV_DIR (that comes
    # first in the result).
    # basename to remove the path.
    # xargs because basename expects one input, but head returns a list.
    FIRST_DIR="$(find "$INPUT_CSV_DIR/" -maxdepth 1 -mindepth 1 | sort | head -n 1 | xargs basename)"
fi

# The directories after FIRST_DIR.
DIRS=""
for d in "$INPUT_CSV_DIR"/*
do
    d=$(basename "$d")
    if [[ "$d" > "$FIRST_DIR" || "$d" == "$FIRST_DIR" ]]
    then
        if [ -z "$DIRS" ]
        then
            DIRS="$d"
        else
            DIRS="$DIRS $d"
        fi
    fi
done

# REF_DIR is some reference directory that we want the current run to be
# compared to.
export REF_DIR
if [ "$REF_DIR" = "" ]
then
    REF_DIR="$FIRST_DIR"
else
    # If a reference directory is set by the user, let's add it to the list of
    # all directories.
    DIRS="$DIRS $REF_DIR"
fi

DUNE="/data/redbull/tezos/_opam/bin/dune"

GPD_DIR="$OCTEZ_DIR/devtools/gas_parameter_diff"

cd "$GPD_DIR" || exit 1

# To put dune in PATH.
eval "$(opam env)"

# This is just to build gas_parameter_diff.
$DUNE exec gas_parameter_diff -- &> /dev/null

PREV_DIR="$LAST_KNOWN_DIR"

for f in "$INPUT_CSV_DIR/$LAST_DIR"/*
do
    b="$(basename "$f")"

    files=$(for d in $DIRS; do local="$INPUT_CSV_DIR/$d/$b"; if [ -f "$local" ]; then echo "$local"; fi; done)

    # Comparing all runs from FIRST_DIR.
    # The parameter expansion syntax ${files:+$files} is used as a trick to
    # output $files without quoting it (because gas_parameter_diff needs a list
    # of files, not one parameter containing all file names), and without making
    # `shellcheck` complain.
    $DUNE exec --no-build gas_parameter_diff -- ${files:+$files} > "$OUTPUT_CSV_DIR"/all_"$b" 2> /dev/null

    # Comparing with the reference and previous runs.
    for current in reference previous
    do
        CURRENT_DIR=""
        if [ "$current" = "reference" ]
        then
            CURRENT_DIR="$REF_DIR"
        else
            CURRENT_DIR="$PREV_DIR"
        fi

        $DUNE exec --no-build --no-print-directory gas_parameter_diff -- "$INPUT_CSV_DIR"/"$CURRENT_DIR"/"$b" "$INPUT_CSV_DIR"/"$LAST_DIR"/"$b" > "$OUTPUT_CSV_DIR"/"$current"_"$b" 2> tmp
        # The parameters with "score" or "T-value" in their name indicate how
        # well the models fits the benchmarks. We care about their values but
        # not much about their variations so we ignore them when looking for
        # regressions. They are however part of the all_*.csv tables computed
        # above.
        grep -v "score\|T-value" tmp > tmp2
        if [ -s tmp2 ]
        then
            {
                echo
                echo "--------------------------------"
                echo "Warning while comparing $b between $LAST_DIR and the $current version $CURRENT_DIR"
                cat tmp2
            } >> "$ALERT_FILE"
            # Save the parameters with alerts in a file. They are in the lines
            # with a '%'.
            grep "%" tmp2 | sed 's/\.//g' | cut -d ' ' -f 4  >> tmp_selection
        fi
        rm -f tmp tmp2
    done

    # Output the lines of parameters with alerts to a dedicated file.
    if [ -s tmp_selection ]
    then
        # Get the header.
        head -n 1 "$OUTPUT_CSV_DIR"/all_"$b" >> "$SELECTION_FILE"
        # Get the line of each parameter.
        # A parameter can appear twice in the alerts, once when comparing with
        # the reference, and another time when comparing with the previous run.
        # That's why we're sorting and uniq-ing.
        for p in $(sort tmp_selection | uniq)
        do
            grep "^$p," "$OUTPUT_CSV_DIR"/all_"$b" >> "$SELECTION_FILE"
        done
    fi
    rm -f tmp_selection
done

cat "$OUTPUT_CSV_DIR"/all_*.csv > "$OUTPUT_CSV_DIR"/all.csv
cat "$OUTPUT_CSV_DIR"/reference_*.csv > "$OUTPUT_CSV_DIR"/reference.csv
cat "$OUTPUT_CSV_DIR"/previous_*.csv > "$OUTPUT_CSV_DIR"/previous.csv

if [ -s "$ALERT_FILE" ]
then
    slack_send_file "$ALERT_FILE" "Some regressions were found :sadparrot:"
else
    slack "No regression :tada:"
fi

if [ -s "$SELECTION_FILE" ]
then
    slack_send_file "$SELECTION_FILE" "CSV comparing all runs on parameters with an alert"
fi

slack_send_file "$OUTPUT_CSV_DIR/all.csv" "CSV comparing all runs"
slack_send_file "$OUTPUT_CSV_DIR/reference.csv" "CSV comparing reference and last runs"
slack_send_file "$OUTPUT_CSV_DIR/previous.csv" "CSV comparing previous and last runs"
