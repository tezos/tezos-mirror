#! /bin/sh

set -e

if [ -z "$1" ] || [ -z "$2" ] || [ -z "$3" ]; then
    echo "Usage: $0 SLACK_TOKEN CHANNEL_ID PIPELINE_URL"
    exit 1;
fi

SLACK_COVERAGE_TOKEN=${1:?}
shift
CHANNEL_ID=${1:?}
shift
CI_PIPELINE_URL=${1:?}
shift


curl --silent \
     -H "Authorization: Bearer $SLACK_COVERAGE_TOKEN" \
     -d "channel=$CHANNEL_ID" \
     -d "text=/!\ Corrupted coverage file found in pipeline: $CI_PIPELINE_URL /!\\" \
     -X POST https://slack.com/api/chat.postMessage
