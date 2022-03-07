#!/usr/bin/env bash

# Load a Tezos indexer PostgreSQL snapshot and estimate the average block.
#
# Arguments:
#
# * $1 = Database dump file.
# * $2 = Start date of the analysis in the YYYY-MM-DD format.
# * $3 = End date of the analysis in the YYYY-MM-DD format.
#
# The script will write a new JSON file with the results of the
# analysis and will also overwrite the average-block.json file.

set -e

if [[ $# -ne 3 ]]; then
    echo "3 parameters are required" >&2
    exit 1
fi

declare -r DATABASE_DUMP_FILE="$1"
declare -r START_DATE="$2"
declare -r END_DATE="$3"
declare -r RESULT_FILE="$START_DATE-to-$END_DATE.json"

pushd analysis-docker-image
docker build . -t tezos-analysis
popd
docker run -d --name=tezos_history_analysis -p 5432:5432 tezos-analysis:latest
bunzip2 -c "$DATABASE_DUMP_FILE" | psql -h localhost -U tezos -a tezos
../../tezos-tps-evaluation estimate-average-block -c postgresql://tezos:tezos@localhost:5432 --start "$START_DATE" --end "$END_DATE" > "$RESULT_FILE"
cp "$RESULT_FILE" "average-block.json"
docker stop tezos_history_analysis
docker rm tezos_history_analysis
