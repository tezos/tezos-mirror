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
  echo "Usage: $0 <psql_indexer_dump_file> <start_date> <end_date>" >&2
  exit 1
fi

declare -r DATABASE_DUMP_FILE="$1"
declare -r START_DATE="$2"
declare -r END_DATE="$3"
declare -r RESULT_FILE="$START_DATE-to-$END_DATE.json"

docker run -d --name tezos_history_analysis -p 5432:5432 -e POSTGRES_HOST_AUTH_METHOD=trust -e POSTGRES_USER=tezos postgres:14-alpine
bunzip2 -c "$DATABASE_DUMP_FILE" | docker exec -i tezos_history_analysis psql -U tezos -a tezos
../../tezos-tps-evaluation-estimate-average-block -a connection-string=postgresql://tezos:tezos@localhost:5432 -a start-date="$START_DATE" -a end-date="$END_DATE" > "$RESULT_FILE"
cp "$RESULT_FILE" "average-block.json"
docker stop tezos_history_analysis
docker rm tezos_history_analysis
