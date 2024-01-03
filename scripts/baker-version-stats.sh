#!/bin/sh

########################################################
# Parse Parameters

FROM_LEVEL=$1
TO_LEVEL=$2
HOST=$3

if [ -z "$HOST" ]; then
  HOST=localhost:8732
fi

if [ -z "$TO_LEVEL" ]; then
  SCRIPT_NAME=$(basename "$0")
  echo "Usage: $SCRIPT_NAME <FROM_LEVEL> <TO_LEVEL> [HOST:PORT]"
  echo
  echo "This script gets the commit hash from the proof of work nonce"
  echo "of levels between FROM_LEVEL and TO_LEVEL."
  echo "It then counts the number of blocks per commit hash."
  echo
  echo "To get the current level, use:"
  echo
  echo "    curl -s http://$HOST/chains/main/blocks/head/header | jq .level"
  echo
  exit 1
fi

########################################################
# Gather Proof of Work Nonces

RAW_FILENAME=$(mktemp --suffix=.txt baker-version-stats-raw.XXXXXXXX)

for i in $(seq "$FROM_LEVEL" "$TO_LEVEL"); do
  #    echo "Fetching $i/$TO_LEVEL..."
  curl -s "http://$HOST/chains/main/blocks/$i/header" | jq -r .proof_of_work_nonce >> "$RAW_FILENAME"
done

########################################################
# Count Statistics

STATS_FILENAME=$(mktemp --suffix=.txt baker-version-stats.XXXXXXXX)
sort "$RAW_FILENAME" | cut -c 1-8 | uniq -c | sort -n > "$STATS_FILENAME"
rm -f "$RAW_FILENAME"

########################################################
# Display Statistics

while read -r LINE; do
  COUNT=$(echo "$LINE" | cut -d ' ' -f 1)
  COMMIT=$(echo "$LINE" | cut -d ' ' -f 2)
  COMMIT_INFO=$(PAGER="cat" git log "$COMMIT" -1 --format=format:'%h %ci%d%n%s' 2> /dev/null)
  echo
  if [ -z "$COMMIT_INFO" ]; then
    echo "$COMMIT (not a commit)"
  else
    echo "$COMMIT_INFO"
  fi
  echo "=> $COUNT blocks"
done < "$STATS_FILENAME"

rm -f "$STATS_FILENAME"
