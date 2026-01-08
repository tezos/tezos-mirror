#!/bin/bash

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

# Query to extract and format months from timestamps
psql -q "$CONN_STRING" << EOF
SELECT
    to_char(date_trunc('month', to_timestamp("timestamp")), 'YYYY-MM') as month,
    COUNT(*) as blocks,
    to_char(MIN(to_timestamp("timestamp")), 'YYYY-MM-DD HH24:MI:SS') as first_block,
    to_char(MAX(to_timestamp("timestamp")), 'YYYY-MM-DD HH24:MI:SS') as last_block
FROM blocks
GROUP BY date_trunc('month', to_timestamp("timestamp"))
ORDER BY month;
EOF
