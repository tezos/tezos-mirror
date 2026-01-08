#!/bin/bash

# A script to benchmark the performance of the eth_getLogs RPC

if ! command -v jq > /dev/null 2>&1; then
  echo "jq is not installed. Please install it to run this script."
  exit 1
fi

# Configuration
NODE_URL=${NODE_URL:-"http://localhost:8545"}
OUTPUT_DIR=${OUTPUT_DIR:-"getlogs_benchmark_results_$(date +%Y%m%d_%H%M%S)"}
CSV_FILE="$OUTPUT_DIR/results.csv"
GRAPH_FILE="$OUTPUT_DIR/response_times.png"

# Define request types (only range_size)
# Can be changed to test different range sizes
declare -a request_sizes=(1 10 100 1000 5000 10000)

usage() {
  cat << EOF
Usage:
  $0 <start_block> <end_block> <max_parallelism> [max_requests]

Arguments:
  start_block     Starting block number for eth_getLogs requests.
  end_block       Ending block number for eth_getLogs requests.
  max_parallelism Maximum number of parallel curl requests.
  max_requests    Optional. Maximum number of requests to generate. Defaults to 120.

Example:
  $0 23239687 23279687 10 100
EOF
  exit 1
}

if [[ "$#" -lt 3 || "$#" -gt 4 ]]; then
  usage
fi

start_block=$1
end_block=$2
max_parallelism=$3
MAX_REQUESTS=${4:-120}

set -euo pipefail

# Create output directory
mkdir -p "$OUTPUT_DIR"

# Initialize CSV file
echo "range_size,response_time_ms,status,num_logs" > "$CSV_FILE"

echo "Generating requests (max $MAX_REQUESTS)..."
declare -a requests_to_run

num_request_types=${#request_sizes[@]}
# Aim for an even distribution, but ensure at least 1 for each type if MAX_REQUESTS is small
target_requests_per_type=$((MAX_REQUESTS / num_request_types))
if ((target_requests_per_type == 0)); then
  target_requests_per_type=1 # Ensure at least one of each type
fi

for current_range_size in "${request_sizes[@]}"; do
  block_range=$((end_block - start_block + 1))
  if ((block_range <= 0)); then
    continue # Skip if block range is invalid
  fi

  # Calculate step to distribute target_requests_per_type evenly
  step=$((block_range / target_requests_per_type))
  if ((step == 0)); then
    step=1 # Ensure at least one request per block if range is too small
  fi

  for ((i = 0; i < target_requests_per_type; i++)); do
    block=$((start_block + (i * step)))
    if ((block > end_block)); then
      break # Stop if we go beyond end_block
    fi
    to_block=$((block + current_range_size - 1))
    requests_to_run+=("$(printf '%d,%d,%d' "$block" "$to_block" "$current_range_size")")
  done
done

# Trim requests_to_run if it exceeds MAX_REQUESTS
if ((${#requests_to_run[@]} > MAX_REQUESTS)); then
  requests_to_run=("${requests_to_run[@]:0:$MAX_REQUESTS}")
fi

echo "Generated ${#requests_to_run[@]} requests."

echo "Executing requests in parallel (max $max_parallelism)..."

# Function to execute a single curl request and measure time
execute_request() {
  local from_block to_block range_size payload start_time http_code end_time duration_ms num_logs response_file
  from_block=$1
  to_block=$2
  range_size=$3

  # Ensure to_block does not exceed end_block
  if ((to_block > end_block)); then
    to_block=$end_block
  fi

  # Comment topic (Transfer) to get all logs
  payload='{"jsonrpc":"2.0","method":"eth_getLogs","params":[{"fromBlock":"0x'$(printf '%x' "$from_block")'","toBlock":"0x'$(printf '%x' "$to_block")'", "topics":["0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef"]}],"id":1}'

  response_file=$(mktemp)

  start_time=$(date +%s%N)
  http_code=$(curl -s -o "$response_file" -w "%{response_code}" -X POST -H "Content-Type: application/json" --data "$payload" "$NODE_URL")
  end_time=$(date +%s%N)

  duration_ms=$(((end_time - start_time) / 1000000)) # Convert nanoseconds to milliseconds

  if [[ "$http_code" -ne 200 ]]; then
    echo "$range_size,$duration_ms,HTTP_ERROR,0" >> "$CSV_FILE" # HTTP error
  else
    # Check for JSON-RPC error in the response body
    if grep -q '"error":' "$response_file"; then
      cat "$response_file"
      echo "$range_size,$duration_ms,JSON_RPC_ERROR,0" >> "$CSV_FILE" # JSON-RPC error
    else
      num_logs=$(jq '.result | length' "$response_file")
      echo "$range_size,$duration_ms,OK,$num_logs" >> "$CSV_FILE"
    fi
  fi
  rm -f "$response_file"
}

export -f execute_request
export NODE_URL
export CSV_FILE
export end_block # Pass end_block to the function for clipping

# Use GNU Parallel for parallel execution and progress bar
printf "%s\n" "${requests_to_run[@]}" | parallel -j "$max_parallelism" --bar --eta --colsep ',' execute_request "{1}" "{2}" "{3}"

echo "All requests completed. Results saved to $CSV_FILE"

# Calculate and print failure statistics
TOTAL_REQUESTS=$(wc -l < "$CSV_FILE")
TOTAL_REQUESTS=$((TOTAL_REQUESTS - 1)) # Subtract header row
SUCCESSFUL_REQUESTS_FILE="$OUTPUT_DIR/successful_requests.csv"
grep ",OK," "$CSV_FILE" > "$SUCCESSFUL_REQUESTS_FILE"
SUCCESSFUL_REQUESTS=$(wc -l < "$SUCCESSFUL_REQUESTS_FILE")
FAILED_REQUESTS=$((TOTAL_REQUESTS - SUCCESSFUL_REQUESTS))

# Sum total logs from successful requests
TOTAL_LOGS=0
if [ -s "$SUCCESSFUL_REQUESTS_FILE" ]; then
  TOTAL_LOGS=$(awk -F, '{s+=$4} END {print s}' "$SUCCESSFUL_REQUESTS_FILE")
fi

echo ""
echo "--- Benchmark Summary ---"
if ((TOTAL_REQUESTS > 0)); then
  FAILURE_PERCENTAGE=$(awk "BEGIN { printf \"%.2f\", ($FAILED_REQUESTS / $TOTAL_REQUESTS) * 100 }")
  echo "Total requests: $TOTAL_REQUESTS"
  echo "Successful requests: $SUCCESSFUL_REQUESTS"
  echo "Failed requests: $FAILED_REQUESTS"
  echo "Failure percentage: $FAILURE_PERCENTAGE%"
  echo "Total logs returned: $TOTAL_LOGS"
else
  echo "No requests were executed."
fi
echo ""

echo "Generating graph..."

# Calculate median response time for each range size
MEDIANS_FILE="$OUTPUT_DIR/medians.csv"
echo "range_size,median_response_time_ms" > "$MEDIANS_FILE"

for size in "${request_sizes[@]}"; do
  # Filter successful requests for the current size, get response times, sort them
  times=$(grep "^$size," "$CSV_FILE" | grep ",OK" | cut -d, -f2 | sort -n)

  # If no successful requests for this size, skip
  if [ -z "$times" ]; then
    continue
  fi

  # Calculate median using awk
  median=$(echo "$times" | awk '
        { arr[NR] = $1 }
        END {
            if (NR == 0) {
                print 0
            } else if (NR % 2) {
                print arr[(NR+1)/2]
            } else {
                print (arr[NR/2] + arr[NR/2+1]) / 2.0
            }
        }')
  echo "$size,$median" >> "$MEDIANS_FILE"
done

echo ""
echo "--- Median Response Times (ms) ---"
column -t -s, "$MEDIANS_FILE"
echo ""

# Generate graph using gnuplot, maybe not that useful
gnuplot -persist << EOF
set terminal pngcairo size 1200,800
set output '$GRAPH_FILE'
set title 'Median eth_getLogs Response Time (Blocks $start_block to $end_block)'
set xlabel 'Block Range Size'
set ylabel 'Median Response Time (ms)'
set style fill solid 0.5
set logscale x 10
set datafile separator ","
set key off

plot '$MEDIANS_FILE' using 1:2 with boxes notitle
EOF

echo "Graph saved to $GRAPH_FILE"
echo "Benchmark complete."
