#!/bin/sh

# -x: echo run commands to stderr (which is redirected to cron_res_errors, see ./cronjob.sh).
# -e: stop on first error.
set -x -e

TODAY=$(date +"%Y%m%d_%H%M")
date +"[%Y-%m-%d %T] Starting benchmarks processes."
cd /data/tezos-benchmarks/tezos
rm -rf _opam
echo "Pulling repository."
git pull
HEADCOMMIT=$(git rev-parse HEAD)
echo -n "HEAD is $HEADCOMMIT"
SNOOP_RESULT_DIR="snoop_results/_snoop_${TODAY}_${HEADCOMMIT}"
cd ..
echo "$SNOOP_RESULT_DIR" > current_run_dir
mkdir "$SNOOP_RESULT_DIR"
cd tezos
date +"[%Y-%m-%d %T] Compiling dependencies."
. "/home/mclaren/.cargo/env"
make BLST_PORTABLE=y build-dev-deps || true
if [ -d _opam/share/zcash-params ]; then echo "zcash params found"; else cp -r ../zcash-params _opam/share/; fi
eval $(opam env)
date +"[%Y-%m-%d %T] Make."
make
date +"[%Y-%m-%d %T] Running benchmarks."
time dune exec tezt/snoop/main.exe -- --verbose
date +"[%Y-%m-%d %T] End of benchmarks processes."
cd ..
mv tezos/_snoop/*_results "$SNOOP_RESULT_DIR"/
chmod +rx "$SNOOP_RESULT_DIR"/*_results
mv current_run_dir last_run_dir
