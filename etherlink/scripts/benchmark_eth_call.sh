#!/bin/bash

# A script to benchmark the performance of the eth_call RPC

# Configuration
NODE_URL=${NODE_URL:-"http://localhost:8545"}
OUTPUT_DIR=${OUTPUT_DIR:-"eth_call_benchmark_results_$(date +%Y%m%d_%H%M%S)"}
CSV_FILE="$OUTPUT_DIR/results.csv"

usage() {
  cat << EOF
Usage:
  $0 <requests_file> <total_requests> <max_parallelism>

Arguments:
  requests_file   Path to a file containing JSON-RPC request payloads, one per line.
  total_requests  Total number of requests to execute.
  max_parallelism Maximum number of parallel curl requests.

Example:
  $0 ../script-inputs/iguana-eth_call-examples.txt 1000 10
EOF
  exit 1
}

# Argument Parsing
if [ "$#" -ne 3 ]; then
  usage
fi

requests_file=$1
total_requests=$2
max_parallelism=$3

if [ ! -f "$requests_file" ]; then
  echo "Error: Requests file not found at '$requests_file'" >&2
  exit 1
fi

set -euo pipefail

# Initialization
mkdir -p "$OUTPUT_DIR"
echo "request_id,response_time_ms,status" > "$CSV_FILE"

# Request Generation
echo "Generating requests..."
declare -a requests_to_run
declare -a request_templates

while IFS= read -r line; do
  if [ -n "$line" ]; then
    request_templates+=("$line")
  fi
done < "$requests_file"
num_templates=${#request_templates[@]}

if ((num_templates == 0)); then
  echo "Error: No request templates found in '$requests_file'" >&2
  exit 1
fi

for ((i = 0; i < total_requests; i++)); do
  requests_to_run+=("${request_templates[i % num_templates]}")
done

echo "Generated ${#requests_to_run[@]} requests."

# Execution

# Function to execute a single curl request and measure time
execute_request() {
  local payload="$1"
  local request_id

  # Safely extract request ID using jq
  if ! request_id=$(jq -r .id <<< "$payload"); then
    echo "Error: Failed to parse request_id from payload. Is jq installed and is the payload valid JSON?" >&2
    echo "Payload: $payload" >&2
    exit 1 # Exit subshell
  fi

  local response_file
  response_file=$(mktemp)
  trap 'rm -f "$response_file"' RETURN

  local start_time
  start_time=$(date +%s%N)

  local http_code
  http_code=$(curl -s -o "$response_file" -w "%{response_code}" -X POST -H "Content-Type: application/json" --data "$payload" "$NODE_URL")

  local end_time
  end_time=$(date +%s%N)

  local duration_ms=$(((end_time - start_time) / 1000000))

  if [[ "$http_code" -ne 200 ]]; then
    echo "$request_id,$duration_ms,HTTP_ERROR_$http_code" >> "$CSV_FILE"
  else
    # Check for JSON-RPC level errors in the response body
    if jq -e '.error' "$response_file" > /dev/null; then
      echo "$request_id,$duration_ms,JSON_RPC_ERROR" >> "$CSV_FILE"
    else
      echo "$request_id,$duration_ms,OK" >> "$CSV_FILE"
    fi
  fi
}

export -f execute_request
export NODE_URL
export CSV_FILE

echo "Executing requests in parallel (max $max_parallelism)..."

# Use GNU Parallel for parallel execution and progress bar
printf "%s\n" "${requests_to_run[@]}" | parallel -j "$max_parallelism" --bar --eta execute_request

echo "All requests completed. Results saved to $CSV_FILE"

# Analysis and Output

# Get unique request IDs from the results
unique_ids=$(sed 1d "$CSV_FILE" | cut -d, -f1 | sort -u)

MEDIANS_FILE="$OUTPUT_DIR/medians.csv"
echo "request_id,median_response_time_ms" > "$MEDIANS_FILE"

for id in $unique_ids; do
  # Filter successful requests for the current id, get response times, sort them
  times=$(grep "^$id," "$CSV_FILE" | grep ",OK$" | cut -d, -f2 | sort -n)

  if [ -z "$times" ]; then
    continue
  fi

  # Calculate median using awk
  median=$(echo "$times" | awk '
        { arr[NR] = $1 }
        END {
            if (NR == 0) {
                print "N/A"
            } else if (NR % 2) {
                print arr[(NR+1)/2]
            } else {
                print (arr[NR/2] + arr[NR/2+1]) / 2.0
            }
        }')
  echo "$id,$median" >> "$MEDIANS_FILE"
done

echo ""
echo "--- Median Response Times (ms) ---"
column -t -s, "$MEDIANS_FILE"
echo ""

echo "Benchmark complete. Full results in $CSV_FILE"
