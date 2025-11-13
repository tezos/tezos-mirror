#!/bin/bash

# Usage: ./dump_month.sh YYYY-MM output_directory
# Example: ./dump_month.sh 2024-01 /path/to/dumps

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
mkdir -p "$OUTPUT_DIR"

DUMP_FILE_PREFIX="teztale_${DATABASE}_dump"

# Convert date to start and end timestamps (epoch)
START_TS=$(date -d "$DATE-01" +%s)
END_TS=$(date -d "$DATE-01 +1 month" +%s)

echo "Dumping data from timestamp $START_TS to $END_TS [$DATABASE]"

echo "Create views for the month's data and related records  [$DATABASE]"
psql -q "$CONN_STRING" << EOF
BEGIN;

-- Drop any existing views
DROP VIEW IF EXISTS monthly_view_blocks CASCADE;
DROP VIEW IF EXISTS monthly_view_blocks_reception CASCADE;
DROP VIEW IF EXISTS monthly_view_operations CASCADE;
DROP VIEW IF EXISTS monthly_view_operations_reception CASCADE;
DROP VIEW IF EXISTS monthly_view_operations_inclusion CASCADE;
DROP VIEW IF EXISTS monthly_view_delegates CASCADE;
DROP VIEW IF EXISTS monthly_view_cycles CASCADE;
DROP VIEW IF EXISTS monthly_view_missing_blocks CASCADE;
DROP VIEW IF EXISTS monthly_view_endorsing_rights CASCADE;
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

-- Create view for related delegates
CREATE VIEW monthly_view_delegates AS
SELECT DISTINCT d.* FROM delegates d
WHERE d.id IN (
    SELECT baker FROM monthly_view_blocks
    UNION
    SELECT endorser FROM monthly_view_operations
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

-- Create view for related endorsing_rights
CREATE VIEW monthly_view_endorsing_rights AS
SELECT er.* FROM endorsing_rights er
WHERE er.level >= (SELECT MIN(level) FROM monthly_view_blocks)
AND er.level <= (SELECT MAX(level) FROM monthly_view_blocks);

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

-- Create a summary view to verify data integrity
CREATE VIEW monthly_view_data_summary AS
SELECT
    (SELECT COUNT(*) FROM monthly_view_blocks) as block_count,
    (SELECT COUNT(*) FROM monthly_view_blocks_reception) as block_reception_count,
    (SELECT COUNT(*) FROM monthly_view_operations) as operation_count,
    (SELECT COUNT(*) FROM monthly_view_operations_reception) as operation_reception_count,
    (SELECT COUNT(*) FROM monthly_view_delegates) as delegate_count,
    (SELECT COUNT(*) FROM monthly_view_cycles) as cycle_count,
    (SELECT COUNT(*) FROM monthly_view_missing_blocks) as missing_block_count,
    (SELECT COUNT(*) FROM monthly_view_endorsing_rights) as endorsing_rights_count,
    (SELECT COUNT(*) FROM monthly_view_nodes) as nodes_count;

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

DROP TABLE IF EXISTS temp_monthly_view_delegates;
CREATE TABLE temp_monthly_view_delegates AS TABLE monthly_view_delegates;

DROP TABLE IF EXISTS temp_monthly_view_cycles;
CREATE TABLE temp_monthly_view_cycles AS TABLE monthly_view_cycles;

DROP TABLE IF EXISTS temp_monthly_view_missing_blocks;
CREATE TABLE temp_monthly_view_missing_blocks AS TABLE monthly_view_missing_blocks;

DROP TABLE IF EXISTS temp_monthly_view_endorsing_rights;
CREATE TABLE temp_monthly_view_endorsing_rights AS TABLE monthly_view_endorsing_rights;

DROP TABLE IF EXISTS temp_monthly_view_nodes;
CREATE TABLE temp_monthly_view_nodes AS TABLE monthly_view_nodes;

COMMIT;
EOF

exit_code_2=$?

if [ $exit_code_2 -ne 0 ]; then
  echo "Error: Failed to create temporary tables from views [$DATABASE]"
  exit 1
fi

# Display summary of the data to be dumped
echo "Data summary before dump [$DATABASE]:"
psql -q "$CONN_STRING" -P pager=off -c "SELECT * FROM monthly_view_data_summary;"

echo "Dump the views [$DATABASE]"
pg_dump \
  "$CONN_STRING" \
  --format plain \
  --column-inserts \
  --file "$OUTPUT_DIR/${DUMP_FILE_PREFIX}_${DATE}.dump" \
  --verbose \
  --no-owner \
  --data-only \
  --table=temp_monthly_view_blocks \
  --table=temp_monthly_view_blocks_reception \
  --table=temp_monthly_view_operations \
  --table=temp_monthly_view_operations_reception \
  --table=temp_monthly_view_operations_inclusion \
  --table=temp_monthly_view_delegates \
  --table=temp_monthly_view_cycles \
  --table=temp_monthly_view_missing_blocks \
  --table=temp_monthly_view_endorsing_rights \
  --table=temp_monthly_view_nodes

exit_code_3=$?

echo "Clean up views [$DATABASE]"
psql -q "$CONN_STRING" << EOF
DROP VIEW IF EXISTS monthly_view_blocks CASCADE;
DROP VIEW IF EXISTS monthly_view_blocks_reception CASCADE;
DROP VIEW IF EXISTS monthly_view_operations CASCADE;
DROP VIEW IF EXISTS monthly_view_operations_reception CASCADE;
DROP VIEW IF EXISTS monthly_view_operations_inclusion CASCADE;
DROP VIEW IF EXISTS monthly_view_delegates CASCADE;
DROP VIEW IF EXISTS monthly_view_cycles CASCADE;
DROP VIEW IF EXISTS monthly_view_missing_blocks CASCADE;
DROP VIEW IF EXISTS monthly_view_endorsing_rights CASCADE;
DROP VIEW IF EXISTS monthly_view_nodes CASCADE;
DROP VIEW IF EXISTS monthly_view_data_summary;

DROP TABLE IF EXISTS temp_monthly_view_blocks;
DROP TABLE IF EXISTS temp_monthly_view_blocks_reception;
DROP TABLE IF EXISTS temp_monthly_view_operations;
DROP TABLE IF EXISTS temp_monthly_view_operations_reception;
DROP TABLE IF EXISTS temp_monthly_view_operations_inclusion;
DROP TABLE IF EXISTS temp_monthly_view_delegates;
DROP TABLE IF EXISTS temp_monthly_view_cycles;
DROP TABLE IF EXISTS temp_monthly_view_missing_blocks;
DROP TABLE IF EXISTS temp_monthly_view_endorsing_rights;
DROP TABLE IF EXISTS temp_monthly_view_nodes;

EOF

if [ $exit_code_3 -ne 0 ]; then
  echo "Error: pg_dump failed with exit code $exit_code_3 [$DATABASE]"
  exit $exit_code_3
fi

echo "Successfully created dump at $OUTPUT_DIR/${DUMP_FILE_PREFIX}_${DATE}.dump"

echo "Dumping schema ${OUTPUT_DIR}/${DUMP_FILE_PREFIX}_${DATE}.schema"
pg_dump --schema-only "${CONN_STRING}" > "${OUTPUT_DIR}/${DUMP_FILE_PREFIX}_${DATE}.schema"

cp "$OUTPUT_DIR/${DUMP_FILE_PREFIX}_${DATE}.dump" "$OUTPUT_DIR/${DUMP_FILE_PREFIX}_${DATE}.dump_tmp_tables"
sed -i 's/temp_monthly_view_//g' "$OUTPUT_DIR/${DUMP_FILE_PREFIX}_${DATE}.dump"

# Check to see if pbzip2 is already on path; if so, set BZIP_BIN appropriately
type -P pbzip2 &> /dev/null && export BZIP_BIN="pbzip2"
# Otherwise, default to standard bzip2 binary
if [ -z "${BZIP_BIN}" ]; then
  export BZIP_BIN="bzip2"
fi

echo "compressing  ${OUTPUT_DIR}/${DUMP_FILE_PREFIX}_${DATE}.{dump,schema,dump_tmp_tables} ..."
${BZIP_BIN} -v "${OUTPUT_DIR}/${DUMP_FILE_PREFIX}_${DATE}.{dump,schema,dump_tmp_tables}"
echo "compression done of ${OUTPUT_DIR}/${DUMP_FILE_PREFIX}_${DATE}.{dump,schema,dump_tmp_tables}"
