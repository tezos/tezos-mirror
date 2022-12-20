#!/bin/sh

#############################################################################
#                                                                           #
# Open Source License                                                       #
# Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                #
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

# This is the top-level script run by a Cron job to benchmark the Tezos gas
# parameters.

# It simply logs the execution of run_all_benchmarks_on_latest_master.sh, and
# then moves the log files to the result directory returned by the latter
# (through the last_run_dir file).

TODAY=$(date +"%Y%m%d_%H%M")

anomaly() {
  date +"[%Y-%m-%d %T] $1. Remember to clean up when you're done: remove current_run_dir, and move cron_res, cron_res_errors, tezos/_snoop/*_results to the corresponding sub-directory of snoop_results." >> anomalies;
  exit 1
}

cd /data/tezos-benchmarks || (anomaly "Unknown benchmarks directory for $TODAY")

# Check that the previous process is over.
if [ -f "cron_res" ]; then
    anomaly "Can't start the benchmarks process for $TODAY: the previous process isn't over or has failed. cron_res, cron_res_errors and current_run_dir should contain clues to investigate the issue"
fi

./run_all_benchmarks_on_latest_master.sh > cron_res 2> cron_res_errors
run_exit_code=$?

# If the benchmarks succeeded, move the log files to their result directory.
if [ $run_exit_code -eq 0 ]
then
    SNOOP_RESULT_DIR="$(cat last_run_dir)"
    mv cron_res cron_res_errors "$SNOOP_RESULT_DIR"/
elif [ -f "current_run_dir" ] && [ -d "$(cat "current_run_dir")" ]
then
    SNOOP_RESULT_DIR="$(cat current_run_dir)"
    mv cron_res cron_res_errors "$SNOOP_RESULT_DIR"/
    anomaly "The benchmarks run for $TODAY has failed. $SNOOP_RESULT_DIR/{cron_res,cron_res_errors} should contain clues to investigate the issue"
else
    anomaly "Can't save log files for $TODAY: the benchmarks run has failed. cron_res and cron_res_errors should contain clues to investigate the issue"
fi
