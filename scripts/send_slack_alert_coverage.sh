#! /bin/sh

set -eu

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
     -X POST https://slack.com/api/chat.postMessage -o slack-response.json
jq --exit-status '.ok' < slack-response.json > /dev/null || {
    echo "Slack notification unsuccessful, response received:";
    cat slack-response.json;
    exit 1;
}
echo "Slack notification posted to channel $CHANNEL_ID"
