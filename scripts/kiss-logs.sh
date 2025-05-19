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

# Define patterns
hit_pattern="Kisscache hit:"
error_pattern="Kisscache error:"
fallback_pattern="Kisscache server unreachable, falling back to fetch using mangled URL"
direct_fail_pattern="Direct download failed"

# Count occurrences
hits=$(grep -cF "$hit_pattern" "$logfile")
errors=$(grep -cF "$error_pattern" "$logfile")
fallbacks=$(grep -cF "$fallback_pattern" "$logfile")
direct_fails=$(grep -cF "$direct_fail_pattern" "$logfile")

echo "Kisscache hits              : $hits"
echo "Kisscache errors            : $errors"
echo "Fallback to mangled URL     : $fallbacks"
echo "Direct download failures    : $direct_fails"
