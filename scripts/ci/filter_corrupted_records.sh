#!/bin/sh

find tezt/records/ -iname \*.json | while read -r record; do
  if ! jq empty < "$record"; then
    echo "$record is corrupted, moving to ${record}.broken"
    mv "$record" "${record}.broken"
  fi
done
