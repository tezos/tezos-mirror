#!/bin/sh

# Check if a filename was provided
if [ $# -ne 1 ]; then
  echo "Usage: $0 <logfile>"
  exit 1
fi

logfile="$1"

# Check if the file exists
if [ ! -f "$logfile" ]; then
  echo "Error: File '$logfile' not found!"
  exit 2
fi

# Define patterns. These must match the strings kiss-fetch.sh actually emits.
hit_pattern="Kisscache hit:"
error_pattern="Kisscache error:"
unreachable_pattern="Warning: Kisscache not reachable"
direct_ok_pattern="Direct download succeeded:"
direct_fail_pattern="Direct download failed:"

# Count occurrences
hits=$(grep -cF "$hit_pattern" "$logfile")
errors=$(grep -cF "$error_pattern" "$logfile")
unreachable=$(grep -cF "$unreachable_pattern" "$logfile")
direct_ok=$(grep -cF "$direct_ok_pattern" "$logfile")
direct_fails=$(grep -cF "$direct_fail_pattern" "$logfile")

# A fallback to a direct download happens whenever the kisscache path is
# skipped or fails (server unreachable, or a "Kisscache error"); each such
# case ends in a "Direct download succeeded/failed" line, so the number of
# fallbacks is their sum. (The previous counter grepped for a string that
# kiss-fetch.sh no longer emits, so it was always 0.)
fallbacks=$((direct_ok + direct_fails))

echo "Kisscache hits              : $hits"
echo "Kisscache errors            : $errors"
echo "Kisscache unreachable       : $unreachable"
echo "Fallback to direct download : $fallbacks"
echo "Direct download failures    : $direct_fails"
