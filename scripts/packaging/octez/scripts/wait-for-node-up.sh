#!/bin/sh

# Simple helper script used by octez-baker systemd service

url="http://localhost:8732"
max_attempts=30
wait_seconds=15

check_service() {
  if command -v curl > /dev/null && curl --output /dev/null --silent --head --fail "$url"; then
    echo "Service is ready"
    exit 0
  else
    echo "Service not yet ready, waiting..."
  fi
}

#shellcheck disable=SC2034
for i in $(seq 1 $max_attempts); do
  check_service
  sleep $wait_seconds
done

echo "Service did not become ready within the specified time."
exit 1
