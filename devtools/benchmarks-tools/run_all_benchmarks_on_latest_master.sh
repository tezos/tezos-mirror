#!/usr/bin/env bash
# Using bash so that we are able to use the time program.

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

# This is a script to benchmark the Tezos gas parameters from a clone of
# https://gitlab.com/tezos/tezos.

# -x: echo run commands to stderr.
# -e: stop on first error.
set -x -e

# Prevent git errors from being reported to the directory of the previous run if
# it failed and wasn't cleaned properly.
rm -f current_run_dir

TODAY=$(date +"%Y%m%d_%H%M")

# Directory variables for the io benchmarks. They must be set manually.
# Data directory. This directory should contain data from a tezos node.
IO_DATA_DIR=${IO_DATA_DIR-""}
# Cache directory. This directory is used to cache context keys during benchmarks. It should
# be cleaned if the data directory changes. It can be cleaned anytime, at the cost of performance.
IO_CACHE_DIR=${IO_CACHE_DIR-""}

# IO benchmarks are run if and only if the [IO_BENCH] environment variable, as well as the directories
# [IO_DATA_DIR] and [IO_CACHE_DIR], are defined.
if [ -n "$IO_BENCH" ]; then
  if [ -n "$IO_DATA_DIR" ] && [ -n "$IO_CACHE_DIR" ]; then
    extra_param=(--io-only --io-cache-dir "${IO_CACHE_DIR}" --io-data-dir "${IO_DATA_DIR}")
  else
    dated_log "Either IO_DATA_DIR or IO_CACHE_DIR is not set for the IO benchmarks. Exiting."
    exit 1
  fi
else
  extra_param=()
fi

dated_log() {
  date +"[%Y-%m-%d %T] $1."
}

dated_log "Starting benchmarks processes"

# Clean _opam to have a fresh dependencies environment and fetch the latest
# commit.
cd /data/tezos-benchmarks/tezos
rm -rf _opam
dated_log "Pulling repository."
git fetch origin
git checkout master
git pull
HEADCOMMIT=$(git describe --always --dirty --long)
dated_log "HEAD is $HEADCOMMIT"

SNOOP_RESULT_DIR="snoop_results/_snoop_${TODAY}_${HEADCOMMIT}"

# Create the result directory and register its name for tools that depend on it.
# Also create a status file for a quick check from human beings.
cd ..
echo "$SNOOP_RESULT_DIR" > current_run_dir
mkdir -p snoop_results
mkdir "$SNOOP_RESULT_DIR"
echo $$ > "$SNOOP_RESULT_DIR"/STARTED

# Build dependencies.
cd tezos
dated_log "Compiling dependencies"
# shellcheck disable=SC1091
. "/home/mclaren/.cargo/env"
# Reset switch
rm -rf ../.opam
opam init --bare --root /data/tezos-benchmarks/.opam --confirm-level=no
# OPAMSOLVERTIMEOUT=0 means that the opam solver won't timeout
make OPAMSOLVERTIMEOUT=0 build-dev-deps
eval "$(opam env)"

# Build Tezos
dated_log "Make"
make clean
# BLST_PORTABLE=y is needed to benchmark BLS instructions
BLST_PORTABLE=y make

# Install DAL setup
dated_log "Install DAL trusted setup"
# required for DAL benchmarks
./scripts/install_dal_trusted_setup.sh

# Run benchmarks.
dated_log "Running benchmarks"
time dune exec tezt/snoop/main.exe -- --verbose "${extra_param[@]}"
dated_log "End of benchmarks run"

# Move results from tezos to their dedicated directory.
cd ..
mv tezos/_snoop/*_results "$SNOOP_RESULT_DIR"/
chmod +rx "$SNOOP_RESULT_DIR"/*_results

# Setup gcloud CLI
export GOOGLE_APPLICATION_CREDENTIALS="/home/mclaren/.gcp/nl-gas-monitoring-bae9373e1224.json"
GCP_PROJECT_ID="nl-gas-monitoring"
GCP_GCS_SNOOP_RESULTS_BUCKET="nl-snoop-playground"
GCP_GCS_SNOOP_RESULTS_PATH="${GCP_GCS_SNOOP_RESULTS_BUCKET}/mclaren"
if [ ! -f ${GOOGLE_APPLICATION_CREDENTIALS} ]; then
  dated_log "Cannot find GCP Service Account JSON file."
  exit 1
fi
gcloud auth activate-service-account --key-file="$GOOGLE_APPLICATION_CREDENTIALS" --quiet
gcloud config set project ${GCP_PROJECT_ID} --quiet

# Save results in the cloud.
dated_log "Uploading results"
aws s3 cp "$SNOOP_RESULT_DIR"/ s3://snoop-playground/mclaren/complete_results/"$SNOOP_RESULT_DIR"/ --recursive
gsutil cp -r "$SNOOP_RESULT_DIR" gs://${GCP_GCS_SNOOP_RESULTS_PATH}/complete_results/"$SNOOP_RESULT_DIR"

dated_log "Uploading CSVs"
aws s3 cp "$SNOOP_RESULT_DIR"/inference_results/ s3://snoop-playground/mclaren/inference_csvs/"$SNOOP_RESULT_DIR"/ --recursive --exclude "*" --include "*.csv"
find "$SNOOP_RESULT_DIR"/inference_results/ -name "*.csv" | gsutil -m cp -I gs://${GCP_GCS_SNOOP_RESULTS_PATH}/inference_csvs/"$SNOOP_RESULT_DIR"/
dated_log "Results and CSVs uploaded"

# Update the directory of the last successful run, and its status.
mv current_run_dir last_run_dir
echo $$ > "$SNOOP_RESULT_DIR"/SUCCESS
aws s3 cp "$SNOOP_RESULT_DIR"/SUCCESS s3://snoop-playground/mclaren/complete_results/"$SNOOP_RESULT_DIR"/
gsutil cp "$SNOOP_RESULT_DIR"/SUCCESS gs://${GCP_GCS_SNOOP_RESULTS_PATH}/complete_results/"$SNOOP_RESULT_DIR"/

dated_log "End of benchmarks processes"
exit 0
