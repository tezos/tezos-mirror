#!/bin/bash

# Run the dune build command in the RPC_toy folder
dune build

# Run the dune exec RPC_toy.exe executable in the background and capture its output
echo "Starting node server"
PATH_TO_RPC_TOY="./RPC_toy.exe"
output_file=$(mktemp)
dune exec $PATH_TO_RPC_TOY > >(tee -a "$output_file" | while read -r line; do echo "[Server response] : $line"; done) 2>&1 &

# Save the PID of the node process
NODE_PID=$!
echo "Node PID: $NODE_PID"

# Sleep for 5 seconds
sleep 5

# Run the curl command in the background
echo "Make self-loop request to server"
curl -s 'localhost:8080/sleep' &

# Save the PID of the curl process
CURL_PID=$!

# Sleep for 5 more seconds
sleep 5

# Kill the curl process
echo "Attempting to cancel the self-loop request"
kill $CURL_PID

# Wait for the curl process to terminate
wait $CURL_PID 2> /dev/null
echo "Cancelled the self-loop request"

# Check if the "Cohttp connection closed" message is in the server output
if grep -q "Cohttp connection closed" "$output_file"; then
  echo "[IMPORTANT] Cohttp connection CLOSED"
else
  echo "[IMPORTANT] Cohttp connection did NOT CLOSE"
fi

# Clean up the temporary file
rm "$output_file"

# Terminate the node process
echo "Attempting to stop the node server"
kill $NODE_PID

# Wait for the node process to terminate
wait $NODE_PID 2> /dev/null
echo "Stopped the node server"
