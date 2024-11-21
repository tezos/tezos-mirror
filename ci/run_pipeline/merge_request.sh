#!/bin/sh

set -eu

# Project ID defaults to tezos/tezos.
PROJECT_ID="${PROJECT_ID:-3836952}"
CI_PROJECT_NAMESPACE="${CI_PROJECT_NAMESPACE:-tezos}"
BRANCH="${BRANCH:-$(git rev-parse --abbrev-ref HEAD)}"

# Compile parameters into a JSON input for GitLab's API.
DATA=$(cat <<EOF
{
  "ref": "$BRANCH",
  "variables": [
    {
      "key": "CI_PIPELINE_SOURCE",
      "value": "merge_request_event"
    },
    {
      "key": "CI_PROJECT_NAMESPACE",
      "value": "$CI_PROJECT_NAMESPACE"
    }
  ]
}
EOF
)

# Tell the user what is going to happen.
cat <<EOF
Will run a before_merging pipeline for:

            PROJECT_ID = $PROJECT_ID (tezos/tezos is 3836952)
  CI_PROJECT_NAMESPACE = $CI_PROJECT_NAMESPACE
                BRANCH = $BRANCH

You can override those values by setting the corresponding environment variables.
EOF

# Ask for the API token without echoing it.
# Asking for the token instead of expecting it from the command-line
# or from an environment variable ensures it does not stay in the shell history.
stty -echo
printf "Confirm by entering your GitLab API token: "
read -r TOKEN
stty echo
printf "\n"

# Perform the API request.
curl --request POST \
     --header 'Content-Type: application/json' \
     --header "Private-Token: $TOKEN" \
     --data "$DATA" \
     "https://gitlab.com/api/v4/projects/$PROJECT_ID/pipeline"
