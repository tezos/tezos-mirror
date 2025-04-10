#!/bin/bash

main() {
  local source_account="$1"
  local slot_index="$2"
  local slot_data="$3"

  if [ -z "$source_account" ] || [ -z "$slot_index" ] || [ -z "$slot_data" ]; then
    echo "Usage: publish_slot <source_account> <slot_index> <slot_data>"
    return 1
  fi

  if [ -z "$DAL_NODE_RPC_PORT" ]; then
    echo "The environment variable DAL_NODE_RPC_PORT is not set or empty"
    return 1
  fi

  local res
  res=$(curl -s -X POST "http://localhost:${DAL_NODE_RPC_PORT}/slots/" \
    -H "Content-Type: application/json" \
    -d "\"$slot_data\"")

  local commitment proof
  commitment=$(echo "$res" | jq -r '.commitment')
  proof=$(echo "$res" | jq -r '.commitment_proof')

  if [[ -z "$commitment" || -z "$proof" ]]; then
    echo "Error: Could not extract commitment or proof from response:"
    echo "$res"
    return 1
  fi

  octez-client --wait none publish dal commitment "$commitment" from "$source_account" for slot "$slot_index" with proof "$proof"
}

main "$@"
