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

# If we're there, this means that there is a directory on the bucket more recent
# than the last one we treated. Let's call it last_dir.
# Check that all the files in last_dir have been uploaded and that the run was
# successful. Otherwise, exit.

# If we're there, this means that last_dir is more recent than last_known_dir,
# and that the former contains the files of a successful run.
# Let's update last_known_dir as soon as possible so that concurrent runs of
# this script are unlikely to step on each other's feet.

# Now fetch all the files from the bucket (they are the inputs), and reset the
# output directory and the file containing the alerts.

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
