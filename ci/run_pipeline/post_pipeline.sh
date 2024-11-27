#!/bin/sh

# This script is the continuation of other scripts in this directory.
# It expects those other scripts to export the DATA variable.

set -eu

PROJECT_ID="${PROJECT_ID:-3836952}"

# The equal character (=) should be aligned with the one that other scripts output.
cat <<EOF
            PROJECT_ID = $PROJECT_ID (tezos/tezos is 3836952)

You can override those values by setting the corresponding environment variables.

EOF

if [ -n "${GITLAB_PRIVATE_TOKEN:-}" ]; then
    cat <<EOF
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
curl --request POST \
     --header 'Content-Type: application/json' \
     --header "Private-Token: $GITLAB_PRIVATE_TOKEN" \
     --data "$DATA" \
     "https://gitlab.com/api/v4/projects/$PROJECT_ID/pipeline"
