#!/usr/bin/env bash

CLIENT='octez-client --mode mockup --base-dir /tmp/mockup'
STACK='{}'

while :; do
  read -r -p "> " COMMAND || exit 0
  if [ "$COMMAND" = "" ]; then
    COMMAND='{}'
  fi
  NEWSTACK=$($CLIENT run michelson code "$COMMAND" on stack "$STACK" | head -n -2 | tail -n +2)
  if [ "$NEWSTACK" = "" ]; then
    NEWSTACK="$STACK"
  else
    STACK="$NEWSTACK"
    echo "$STACK"
  fi
done
