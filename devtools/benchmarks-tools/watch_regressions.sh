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

ALERT_FILE="$OCTEZ_DIR/alerts"

rm -f "$ALERT_FILE"

# The first directory serves as reference point.
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

DUNE="/data/redbull/tezos/_opam/bin/dune"

GPD_DIR="$OCTEZ_DIR/devtools/gas_parameter_diff"

cd "$GPD_DIR" || exit 1

# To put dune in PATH.
eval "$(opam env)"

# This is just to build gas_parameter_diff.
$DUNE exec gas_parameter_diff -- 2> /dev/null

PREV_DIR="$LAST_KNOWN_DIR"

for f in "$INPUT_CSV_DIR/$LAST_DIR"/*
do
    b="$(basename "$f")"

    # Comparing all runs
    $DUNE exec --no-build gas_parameter_diff -- "$INPUT_CSV_DIR"/*/"$b" > "$OUTPUT_CSV_DIR"/all_"$b" 2> /dev/null

    # Comparing with the first and previous runs.
    for reference in first previous
    do
        REFERENCE_DIR=""
        if [ "$reference" = "first" ]
        then
            REFERENCE_DIR="$FIRST_DIR"
        else
            REFERENCE_DIR="$PREV_DIR"
        fi

        $DUNE exec --no-build --no-print-directory gas_parameter_diff -- "$INPUT_CSV_DIR"/"$REFERENCE_DIR"/"$b" "$INPUT_CSV_DIR"/"$LAST_DIR"/"$b" > "$OUTPUT_CSV_DIR"/"$reference"_"$b" 2> tmp
        # The parameters with "score" in their name indicate how well the models
        # fits the benchmarks. We care about their values but not much about
        # their variations so we ignore them when looking for regressions. They
        # are however part of the all_*.csv tables computed above.
        grep -v score tmp > tmp2
        if [ -s tmp2 ]
        then
            {
                echo
                echo "--------------------------------"
                echo "Warning while comparing $b between $LAST_DIR and the $reference version $REFERENCE_DIR"
                cat tmp2
            } >> "$ALERT_FILE"
        fi
        rm -f tmp tmp2
    done
done

cat "$OUTPUT_CSV_DIR"/all_*.csv > "$OUTPUT_CSV_DIR"/all.csv
cat "$OUTPUT_CSV_DIR"/first_*.csv > "$OUTPUT_CSV_DIR"/first.csv
cat "$OUTPUT_CSV_DIR"/previous_*.csv > "$OUTPUT_CSV_DIR"/previous.csv

if [ -s "$ALERT_FILE" ]
then
    slack_send_file "$ALERT_FILE" "Some regressions were found :sadparrot:"
else
    slack "No regression :tada:"
fi

slack_send_file "$OUTPUT_CSV_DIR/all.csv" "CSV comparing all runs"
slack_send_file "$OUTPUT_CSV_DIR/first.csv" "CSV comparing first and last runs"
slack_send_file "$OUTPUT_CSV_DIR/previous.csv" "CSV comparing previous and last runs"
