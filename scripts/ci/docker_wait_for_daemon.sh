#!/bin/sh
set -eu

timeout=600 # 10 minutes
starttime=$(date +%s)

until docker info >/dev/null 2>&1 || [ $(($(date +%s)-starttime)) -gt ${timeout} ]; do
  echo "Waiting for docker daemon... "
  sleep 3
done

if [ $(($(date +%s)-starttime)) -gt ${timeout} ]; then
  echo "Error: Timeout waiting for docker daemon"
  exit 1
fi

echo "Docker daemon available!"
