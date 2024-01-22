#!/bin/sh
NOW=$(date +"%Y-%m-%dT%H-%M-%S")
node scripts/run_benchmarks.js >> "$OUTPUT/stdout_$NOW"  2>&1