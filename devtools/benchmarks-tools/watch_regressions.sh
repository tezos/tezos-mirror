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

INPUT_CSV_DIR="$OCTEZ_DIR/input_csvs"
OUTPUT_CSV_DIR="$OCTEZ_DIR/output_csvs"

rm -rf "$INPUT_CSV_DIR"
mkdir -p "$INPUT_CSV_DIR"
aws s3 sync s3://snoop-playground/mclaren/inference_csvs/snoop_results/ "$INPUT_CSV_DIR"/

rm -rf "$OUTPUT_CSV_DIR"
mkdir -p "$OUTPUT_CSV_DIR"

ALERT_FILE="$OCTEZ_DIR/alerts"

rm -f "$ALERT_FILE"

# Fetch the first directory that we'll use as a reference.

# Check that gas_parameter_diff compiles. We'll use it to make diffs between the
# inference results.

# For each CSV file in the new directory last_dir:
# * compare the parameter values in the file on all runs;
# * compare the parameter values in the file with the first run;
# * compare the parameter values in the file with the previous run (i.e.
#   last_known_dir);

# The steps above will create as many files as there are in last_dir for each
# comparison (all, first, and previous). Concatenate the results into a single
# file per comparison.
