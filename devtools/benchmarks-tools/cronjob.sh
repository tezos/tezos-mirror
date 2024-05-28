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
# Also, it sends some information to the Slack #gas-benchmarks-reports private
# channel.

TODAY=$(date +"%Y%m%d_%H%M")

# /data/tezos-benchmarks/slack_token contains the token that authorizes to send
# messages using the gas-benchmarks-reports Slack application.
# The file can only be read by mclaren, its owner. This is to prevent anybody to
# use the application to send messages.
# C04HZHR11DW is the identifier for the #gas-benchmarks-reports channel (it can
# be found in its URL).
slack() {
  curl -X POST -H 'Authorization: Bearer '"$(cat /data/tezos-benchmarks/slack_token)" -H 'Content-type: application/json; charset=utf-8' --data "{\"channel\":\"C04HZHR11DW\",\"text\":\"$1\"}" https://tezos-dev.slack.com/api/chat.postMessage
}

anomaly() {
  date +"[%Y-%m-%d %T] $1." >> anomalies
  slack "Something went wrong with \`$TODAY\`'s run :cry:\nThe message below was returned; the referred files can be found at \`163.172.52.82:/data/tezos-benchmarks\`. Remember to clean up for the next run when you're done (see the cleaning procedure in the <https://gitlab.com/tezos/tezos/-/blob/master/devtools/benchmarks-tools/README.md#cleaning-up-after-a-failure|README>).\n** $1."
  exit 1
}

slack "Hi everyone!\nA new run of the benchmarks has started for \`$TODAY\` :alarm_clock:\nIf you don't hear back from me by tomorrow, this probably means that something went very wrong."

cd /data/tezos-benchmarks || (anomaly "Unknown benchmarks directory for \`$TODAY\`")

# Check that the previous process is over.
if [ -f "cron_res" ]; then
  anomaly "Can't start the benchmarks process for \`$TODAY\`: the previous process isn't over or has failed. \`cron_res\`, \`cron_res_errors\` and \`current_run_dir\` should contain clues to investigate the issue"
fi

./run_all_benchmarks_on_latest_master.sh > cron_res 2> cron_res_errors
run_exit_code=$?

# If the benchmarks succeeded, move the log files to their result directory.
if [ $run_exit_code -eq 0 ]; then
  SNOOP_RESULT_DIR="$(cat last_run_dir)"
  mv cron_res cron_res_errors "$SNOOP_RESULT_DIR"/
  slack "The run of the benchmarks for \`$TODAY\` has been successfull :tada:\nResults can be found at \`163.172.52.82:$SNOOP_RESULT_DIR\`."
elif [ -f "current_run_dir" ] && [ -d "$(cat "current_run_dir")" ]; then
  SNOOP_RESULT_DIR="$(cat current_run_dir)"
  mv cron_res cron_res_errors "$SNOOP_RESULT_DIR"/
  anomaly "The benchmarks run for \`$TODAY\` has failed. \`$SNOOP_RESULT_DIR/{cron_res,cron_res_errors}\` should contain clues to investigate the issue"
else
  anomaly "Can't save log files for \`$TODAY\`: the benchmarks run has failed. \`cron_res\` and \`cron_res_errors\` should contain clues to investigate the issue"
fi
