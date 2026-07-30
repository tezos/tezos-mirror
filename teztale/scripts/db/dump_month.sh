#!/bin/bash

# Usage: ./dump_month.sh YYYY-MM output_directory
# Example: ./dump_month.sh 2024-01 /path/to/dumps
#
# The dump is streamed through a compressor: the uncompressed SQL (tens of
# GB for recent months) never lands on any disk. Artifacts are staged on
# local disk (LOCAL_DIR) and only the final .bz2 files are copied to
# output_directory, which may be a FUSE-mounted object store (s3fs):
# writing large files there directly stages them in the local cache and
# fails with ENOSPC once the backing disk is full.
#
# Environment variables:
#   TEZTALE_CONFIG  — teztale config file (default /nomadic/conf/teztale-config.json)
#   LOCAL_DIR       — local staging directory (default ~/teztale_exports/staging)
#   FORCE           — set to 1 to re-dump a month whose outputs already exist
#   ALLOW_UNCOVERED — set to 1 to proceed even if the DB contains tables not
#                     covered by the monthly views (their data would be
#                     missing from the dump)

if [ "$#" -ne 2 ]; then
  echo "Usage: $0 YYYY-MM output_directory"
  exit 1
fi

# Configuration
CONFIG_FILE="${TEZTALE_CONFIG:-/nomadic/conf/teztale-config.json}"
if [ ! -f "$CONFIG_FILE" ]; then
  echo "Error: Config file not found at $CONFIG_FILE"
  exit 1
fi

CONN_STRING=$(jq -r '.db' "$CONFIG_FILE")
if [ -z "$CONN_STRING" ] || [ "$CONN_STRING" == "null" ]; then
  echo "Error: Failed to get database connection string from config"
  exit 1
fi

DATABASE=$(echo "${CONN_STRING}" | awk -F'/' '{print $NF}' | awk -F'\\?' '{print $1}')

DATE=$1
OUTPUT_DIR=$2
LOCAL_DIR="${LOCAL_DIR:-$HOME/teztale_exports/staging}"
mkdir -p "$OUTPUT_DIR" "$LOCAL_DIR"

DUMP_FILE_PREFIX="teztale_${DATABASE}_dump"
F_DUMP="${DUMP_FILE_PREFIX}_${DATE}.dump.bz2"           # table names rewritten (restore-ready)
F_TMP="${DUMP_FILE_PREFIX}_${DATE}.dump_tmp_tables.bz2" # raw temp_monthly_view_* names
F_SCHEMA="${DUMP_FILE_PREFIX}_${DATE}.schema.bz2"

# Idempotency: skip months already exported (FORCE=1 to re-dump)
if [ "${FORCE:-0}" != "1" ] && [ -s "$OUTPUT_DIR/$F_DUMP" ] && [ -s "$OUTPUT_DIR/$F_TMP" ] && [ -s "$OUTPUT_DIR/$F_SCHEMA" ]; then
  echo "Skipping $DATE: all three outputs already exist in $OUTPUT_DIR (set FORCE=1 to redo)"
  exit 0
fi

# Disk guard on the LOCAL staging dir (largest month so far stages ~11GB)
REQUIRED_GB="${REQUIRED_GB:-30}"
AVAIL_GB=$(df -Pk "$LOCAL_DIR" | awk 'NR==2 {print int($4/1048576)}')
if [ -n "$AVAIL_GB" ] && [ "$AVAIL_GB" -lt "$REQUIRED_GB" ]; then
  echo "Error: need ${REQUIRED_GB}G free in $LOCAL_DIR, have ${AVAIL_GB}G"
  exit 1
fi

# Convert date to start and end timestamps (epoch)
# Use GNU date if available (gdate on macOS), otherwise plain date
if command -v gdate > /dev/null 2>&1; then
  DATE_BIN="gdate"
else
  DATE_BIN="date"
fi

START_TS=$("$DATE_BIN" -d "$DATE-01" +%s)
END_TS=$("$DATE_BIN" -d "$DATE-01 +1 month" +%s)

echo "Dumping data from timestamp $START_TS to $END_TS [$DATABASE]"

# Feature-detect optional tables so the script also works against older schemas.
# dal_shard_assignments was added later; absent on pre-DAL teztale DBs.
HAS_DAL_SHARDS=$(psql -tA "$CONN_STRING" -c \
  "SELECT to_regclass('public.dal_shard_assignments') IS NOT NULL;" 2> /dev/null |
  tr -d '[:space:]')

if [ "$HAS_DAL_SHARDS" = "t" ]; then
  echo "[$DATABASE] dal_shard_assignments present — including in dump"
  SQL_DAL_DROP_VIEW="DROP VIEW IF EXISTS monthly_view_dal_shard_assignments CASCADE;"
  SQL_DAL_CREATE_VIEW="CREATE VIEW monthly_view_dal_shard_assignments AS
SELECT dsa.* FROM dal_shard_assignments dsa
JOIN monthly_view_endorsing_rights er ON er.id = dsa.endorsing_right;"
  SQL_DAL_SUMMARY="    (SELECT COUNT(*) FROM temp_monthly_view_dal_shard_assignments) as dal_shard_assignments_count,"
  SQL_DAL_TEMP_TABLE="DROP TABLE IF EXISTS temp_monthly_view_dal_shard_assignments;
CREATE TABLE temp_monthly_view_dal_shard_assignments AS TABLE monthly_view_dal_shard_assignments;"
  SQL_DAL_DROP_TEMP="DROP TABLE IF EXISTS temp_monthly_view_dal_shard_assignments;"
else
  echo "[$DATABASE] dal_shard_assignments not found — skipping DAL view (older schema)"
  SQL_DAL_DROP_VIEW=""
  SQL_DAL_CREATE_VIEW=""
  SQL_DAL_SUMMARY=""
  SQL_DAL_TEMP_TABLE=""
  SQL_DAL_DROP_TEMP=""
fi

# Completeness guard: every base table in the DB must be covered by a monthly
# view below, otherwise its data would be silently missing from the dump.
COVERED_TABLES=(blocks blocks_reception cycles delegates endorsing_rights missing_blocks nodes operations operations_inclusion operations_reception)
if [ "$HAS_DAL_SHARDS" = "t" ]; then
  COVERED_TABLES+=(dal_shard_assignments)
fi
ACTUAL_TABLES=$(psql -At "$CONN_STRING" -c \
  "SELECT tablename FROM pg_tables WHERE schemaname='public' AND tablename NOT LIKE 'temp_monthly_view_%' ORDER BY 1")
UNCOVERED=$(comm -23 <(echo "$ACTUAL_TABLES" | sort) <(printf '%s\n' "${COVERED_TABLES[@]}" | sort))
if [ -n "$UNCOVERED" ]; then
  echo "ERROR: tables present in the DB but NOT covered by the monthly views:"
  echo "$UNCOVERED"
  echo "Their data would be MISSING from the dump. Add views for them (or set ALLOW_UNCOVERED=1 to override)."
  if [ "${ALLOW_UNCOVERED:-0}" != "1" ]; then
    exit 1
  fi
fi

# Check to see if pbzip2 is already on path; if so, set BZIP_BIN appropriately
# Otherwise, default to standard bzip2 binary
if type -P pbzip2 > /dev/null 2>&1; then
  BZIP_BIN="pbzip2"
else
  BZIP_BIN="bzip2"
fi

echo "Create views for the month's data and related records  [$DATABASE]"
psql -q "$CONN_STRING" << EOF
BEGIN;

-- Drop any existing views
DROP VIEW IF EXISTS monthly_view_blocks CASCADE;
DROP VIEW IF EXISTS monthly_view_blocks_reception CASCADE;
DROP VIEW IF EXISTS monthly_view_operations CASCADE;
DROP VIEW IF EXISTS monthly_view_operations_reception CASCADE;
DROP VIEW IF EXISTS monthly_view_operations_inclusion CASCADE;
DROP VIEW IF EXISTS monthly_view_endorsing_rights CASCADE;
${SQL_DAL_DROP_VIEW}
DROP VIEW IF EXISTS monthly_view_delegates CASCADE;
DROP VIEW IF EXISTS monthly_view_cycles CASCADE;
DROP VIEW IF EXISTS monthly_view_missing_blocks CASCADE;
DROP VIEW IF EXISTS monthly_view_nodes CASCADE;

-- Create view for blocks in the specified month
CREATE VIEW monthly_view_blocks AS
SELECT * FROM blocks
WHERE "timestamp" >= $START_TS
AND "timestamp" < $END_TS;

-- Create view for related blocks_reception
CREATE VIEW monthly_view_blocks_reception AS
SELECT block_reception.* FROM blocks_reception block_reception
INNER JOIN monthly_view_blocks mb ON block_reception.block = mb.id;

-- Create view for related operations through operations_inclusion
CREATE VIEW monthly_view_operations AS
SELECT DISTINCT ops.* FROM operations ops
INNER JOIN operations_inclusion oi ON ops.id = oi.operation
INNER JOIN monthly_view_blocks mb ON oi.block = mb.id;

-- Create view for related operations_reception
CREATE VIEW monthly_view_operations_reception AS
SELECT ops_reception.* FROM operations_reception ops_reception
INNER JOIN monthly_view_operations mo ON ops_reception.operation = mo.id;

-- Create view for related operations_inclusion
CREATE VIEW monthly_view_operations_inclusion AS
SELECT ops_inclusion.* FROM operations_inclusion ops_inclusion
INNER JOIN monthly_view_blocks mb ON ops_inclusion.block = mb.id;

-- Create view for related endorsing_rights
CREATE VIEW monthly_view_endorsing_rights AS
SELECT er.* FROM endorsing_rights er
WHERE er.level >= (SELECT MIN(level) FROM monthly_view_blocks)
AND er.level <= (SELECT MAX(level) FROM monthly_view_blocks);

-- Create view for related DAL shard assignments (only when the table exists)
${SQL_DAL_CREATE_VIEW}

-- Create view for related delegates
CREATE VIEW monthly_view_delegates AS
SELECT DISTINCT d.* FROM delegates d
WHERE d.id IN (
    SELECT baker FROM monthly_view_blocks
    UNION
    SELECT endorser FROM monthly_view_operations
    UNION
    SELECT delegate FROM monthly_view_endorsing_rights
);

-- Create view for related cycles
CREATE VIEW monthly_view_cycles AS
SELECT DISTINCT c.* FROM cycles c
WHERE c.level IN (
    SELECT level FROM monthly_view_blocks
);

-- Create view for related missing_blocks
CREATE VIEW monthly_view_missing_blocks AS
SELECT mb.* FROM missing_blocks mb
WHERE mb.level >= (SELECT MIN(level) FROM monthly_view_blocks)
AND mb.level <= (SELECT MAX(level) FROM monthly_view_blocks);

-- Create view for related nodes
CREATE VIEW monthly_view_nodes AS
SELECT DISTINCT n.* FROM nodes n
WHERE n.id IN (
    SELECT source FROM monthly_view_blocks_reception
    UNION
    SELECT source FROM monthly_view_operations_reception
    UNION
    SELECT source FROM monthly_view_missing_blocks
);

COMMIT;
EOF

exit_code_1=$?
if [ $exit_code_1 -ne 0 ]; then
  echo "Error: Failed to create views [$DATABASE]"
  exit 1
fi

echo "Create materialized tables from views [$DATABASE]"

psql -q "$CONN_STRING" << EOF
BEGIN;

-- Create materialized tables from views
DROP TABLE IF EXISTS temp_monthly_view_blocks;
CREATE TABLE temp_monthly_view_blocks AS TABLE monthly_view_blocks;

DROP TABLE IF EXISTS temp_monthly_view_blocks_reception;
CREATE TABLE temp_monthly_view_blocks_reception AS TABLE monthly_view_blocks_reception;

DROP TABLE IF EXISTS temp_monthly_view_operations;
CREATE TABLE temp_monthly_view_operations AS TABLE monthly_view_operations;

DROP TABLE IF EXISTS temp_monthly_view_operations_reception;
CREATE TABLE temp_monthly_view_operations_reception AS TABLE monthly_view_operations_reception;

DROP TABLE IF EXISTS temp_monthly_view_operations_inclusion;
CREATE TABLE temp_monthly_view_operations_inclusion AS TABLE monthly_view_operations_inclusion;

DROP TABLE IF EXISTS temp_monthly_view_endorsing_rights;
CREATE TABLE temp_monthly_view_endorsing_rights AS TABLE monthly_view_endorsing_rights;

${SQL_DAL_TEMP_TABLE}

DROP TABLE IF EXISTS temp_monthly_view_delegates;
CREATE TABLE temp_monthly_view_delegates AS TABLE monthly_view_delegates;

DROP TABLE IF EXISTS temp_monthly_view_cycles;
CREATE TABLE temp_monthly_view_cycles AS TABLE monthly_view_cycles;

DROP TABLE IF EXISTS temp_monthly_view_missing_blocks;
CREATE TABLE temp_monthly_view_missing_blocks AS TABLE monthly_view_missing_blocks;

DROP TABLE IF EXISTS temp_monthly_view_nodes;
CREATE TABLE temp_monthly_view_nodes AS TABLE monthly_view_nodes;

COMMIT;
EOF

exit_code_2=$?

if [ $exit_code_2 -ne 0 ]; then
  echo "Error: Failed to create temporary tables from views [$DATABASE]"
  exit 1
fi

# Display summary of the data to be dumped. Counting the materialized tables
# is cheap (seq scans); counting the views would re-run every expensive join.
echo "Data summary before dump [$DATABASE]:"
psql -q "$CONN_STRING" -P pager=off -c "
SELECT
    (SELECT COUNT(*) FROM temp_monthly_view_blocks) as block_count,
    (SELECT COUNT(*) FROM temp_monthly_view_blocks_reception) as block_reception_count,
    (SELECT COUNT(*) FROM temp_monthly_view_operations) as operation_count,
    (SELECT COUNT(*) FROM temp_monthly_view_operations_reception) as operation_reception_count,
    (SELECT COUNT(*) FROM temp_monthly_view_endorsing_rights) as endorsing_rights_count,
${SQL_DAL_SUMMARY}
    (SELECT COUNT(*) FROM temp_monthly_view_delegates) as delegate_count,
    (SELECT COUNT(*) FROM temp_monthly_view_cycles) as cycle_count,
    (SELECT COUNT(*) FROM temp_monthly_view_missing_blocks) as missing_block_count,
    (SELECT COUNT(*) FROM temp_monthly_view_nodes) as nodes_count;"

# Build --table args from what was actually materialized, so a future added
# view can never be forgotten in a hardcoded list.
TABLE_ARGS=()
while IFS= read -r t; do
  TABLE_ARGS+=("--table=$t")
done < <(psql -At "$CONN_STRING" -c \
  "SELECT tablename FROM pg_tables WHERE schemaname='public' AND tablename LIKE 'temp_monthly_view_%' ORDER BY 1")
echo "Dumping ${#TABLE_ARGS[@]} tables: ${TABLE_ARGS[*]}"

echo "Dump the views, streamed into $BZIP_BIN (no uncompressed file is ever written) [$DATABASE]"
set -o pipefail
pg_dump \
  "$CONN_STRING" \
  --format plain \
  --column-inserts \
  --verbose \
  --no-owner \
  --data-only \
  "${TABLE_ARGS[@]}" |
  "${BZIP_BIN}" > "$LOCAL_DIR/$F_TMP"
exit_code_3=$?
set +o pipefail

echo "Clean up views [$DATABASE]"
psql -q "$CONN_STRING" << EOF
DROP VIEW IF EXISTS monthly_view_blocks CASCADE;
DROP VIEW IF EXISTS monthly_view_blocks_reception CASCADE;
DROP VIEW IF EXISTS monthly_view_operations CASCADE;
DROP VIEW IF EXISTS monthly_view_operations_reception CASCADE;
DROP VIEW IF EXISTS monthly_view_operations_inclusion CASCADE;
DROP VIEW IF EXISTS monthly_view_endorsing_rights CASCADE;
${SQL_DAL_DROP_VIEW}
DROP VIEW IF EXISTS monthly_view_delegates CASCADE;
DROP VIEW IF EXISTS monthly_view_cycles CASCADE;
DROP VIEW IF EXISTS monthly_view_missing_blocks CASCADE;
DROP VIEW IF EXISTS monthly_view_nodes CASCADE;
DROP VIEW IF EXISTS monthly_view_data_summary;

DROP TABLE IF EXISTS temp_monthly_view_blocks;
DROP TABLE IF EXISTS temp_monthly_view_blocks_reception;
DROP TABLE IF EXISTS temp_monthly_view_operations;
DROP TABLE IF EXISTS temp_monthly_view_operations_reception;
DROP TABLE IF EXISTS temp_monthly_view_operations_inclusion;
DROP TABLE IF EXISTS temp_monthly_view_endorsing_rights;
${SQL_DAL_DROP_TEMP}
DROP TABLE IF EXISTS temp_monthly_view_delegates;
DROP TABLE IF EXISTS temp_monthly_view_cycles;
DROP TABLE IF EXISTS temp_monthly_view_missing_blocks;
DROP TABLE IF EXISTS temp_monthly_view_nodes;

EOF

if [ $exit_code_3 -ne 0 ]; then
  echo "Error: pg_dump pipeline failed with exit code $exit_code_3 [$DATABASE]"
  rm -f "$LOCAL_DIR/$F_TMP"
  exit $exit_code_3
fi

echo "Producing restore-ready variant (table names rewritten, streamed)"
set -o pipefail
if ! "${BZIP_BIN}" -dc "$LOCAL_DIR/$F_TMP" | sed 's/temp_monthly_view_//g' | "${BZIP_BIN}" > "$LOCAL_DIR/$F_DUMP"; then
  echo "Error: rename/recompress pipeline failed"
  rm -f "$LOCAL_DIR/$F_DUMP"
  exit 1
fi

echo "Dumping schema to $F_SCHEMA"
if ! pg_dump --schema-only "${CONN_STRING}" | "${BZIP_BIN}" > "$LOCAL_DIR/$F_SCHEMA"; then
  echo "Error: schema dump failed"
  rm -f "$LOCAL_DIR/$F_SCHEMA"
  exit 1
fi
set +o pipefail

echo "Publishing to $OUTPUT_DIR"
for f in "$F_TMP" "$F_DUMP" "$F_SCHEMA"; do
  if ! cp "$LOCAL_DIR/$f" "$OUTPUT_DIR/$f"; then
    echo "Error: copy of $f to $OUTPUT_DIR failed"
    exit 1
  fi
  src_size=$(wc -c < "$LOCAL_DIR/$f")
  dst_size=$(wc -c < "$OUTPUT_DIR/$f")
  if [ "$src_size" -ne "$dst_size" ]; then
    echo "Error: size mismatch for $f (local $src_size vs destination $dst_size)"
    exit 1
  fi
  echo "  $f : $dst_size bytes OK"
done
rm -f "$LOCAL_DIR/$F_TMP" "$LOCAL_DIR/$F_DUMP" "$LOCAL_DIR/$F_SCHEMA"

echo "Successfully exported $DATE to $OUTPUT_DIR"
