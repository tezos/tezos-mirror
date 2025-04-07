#!/bin/bash

echo "start shell service"

# Start a long-running process to keep the container pipes open
# shellcheck disable=SC2217
sleep infinity < /proc/1/fd/0 > /proc/1/fd/1 2>&1 &

# Wait a bit before retrieving the PID
sleep 1

# Save the long-running PID on file
echo $! > /container-pipes-pid

# Start systemd as PID 1
exec /usr/lib/systemd/systemd
