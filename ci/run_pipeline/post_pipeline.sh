#!/bin/sh

# This script is the continuation of other scripts in this directory.
# It expects those other scripts to export the DATA variable.

set -eu

PROJECT_ID="${PROJECT_ID:-3836952}"

# The equal character (=) should be aligned with the one that other scripts output.
cat << EOF
                         PROJECT_ID = "$PROJECT_ID" (tezos/tezos is 3836952, nomadic-labs/tezos is 9487506)

You can override those values by setting the corresponding environment variables.

EOF

if [ -n "${GITLAB_PRIVATE_TOKEN:-}" ]; then
  cat << EOF
WARNING: GitLab private token was read from GITLAB_PRIVATE_TOKEN.
Please double check that this environment variable was not stored
in your shell history file (e.g. ~/.bash_history or ~/.histfile).
Note that it may only be written there after you close your terminal.
EOF
else
  # Ask for the API token without echoing it.
  stty -echo
  printf "Confirm by entering your GitLab API token: "
  read -r GITLAB_PRIVATE_TOKEN
  stty echo
  printf "\n"
fi

# Perform the API request.
RESPONSE="$(curl --silent --request POST \
  --header 'Content-Type: application/json' \
  --header "Private-Token: $GITLAB_PRIVATE_TOKEN" \
  --data "$DATA" \
  "https://gitlab.com/api/v4/projects/$PROJECT_ID/pipeline")"

# Parse the response.
if ! command -v jq > /dev/null 2>&1; then
  cat << EOF

GitLab's response:

$RESPONSE

Tip: install jq to improve the readability of this result.
EOF
elif [ "$(echo "$RESPONSE" | jq 'has("web_url")')" != "true" ]; then
  cat << EOF

GitLab's response:

$RESPONSE

Failed to find web_url in response; the pipeline was probably not created.
EOF
else
  ID="$(echo "$RESPONSE" | jq -r ".id")"
  URL="$(echo "$RESPONSE" | jq -r ".web_url")"
  cat << EOF

Created pipeline $ID at: $URL
EOF
fi
