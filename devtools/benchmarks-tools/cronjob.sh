#!/bin/sh

cd /data/tezos-benchmarks
./run_all_benchmarks_on_latest_master.sh > cron_res 2> cron_res_errors
SNOOP_RESULT_DIR="$(cat last_run_dir)"
mv cron_res cron_res_errors "$SNOOP_RESULT_DIR"/
