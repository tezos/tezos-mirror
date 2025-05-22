#!/bin/bash

set -eu

# Calls the Slack Webhook that will trigger a notification.

# Notification will contain link to the scan report, total of
# vulnerabilities and breakdown by severity levels.

# parse the report for key info

REPORT_FILE="gl-container-scanning-report.json"

# Check that the file exists and is valid JSON
jq empty "$REPORT_FILE" || {
  echo "❌ ERROR: File is missing or not valid JSON." >&2
  exit 1
}

# Ensure `.vulnerabilities` is present and is an array
if ! jq -e 'has("vulnerabilities") and (.vulnerabilities | type == "array")' "$REPORT_FILE" > /dev/null; then
  echo "❌ ERROR: .vulnerabilities field is missing or not an array." >&2
  exit 1
fi

# Proceed with extracting counts
vulnerabilities=$(jq '.vulnerabilities | length' "$REPORT_FILE")
critical=$(jq '[.vulnerabilities[] | select(.severity == "Critical")] | length' "$REPORT_FILE")
high=$(jq '[.vulnerabilities[] | select(.severity == "High")] | length' "$REPORT_FILE")
medium=$(jq '[.vulnerabilities[] | select(.severity == "Medium")] | length' "$REPORT_FILE")
low=$(jq '[.vulnerabilities[] | select(.severity == "Low")] | length' "$REPORT_FILE")
info=$(jq '[.vulnerabilities[] | select(.severity == "Info")] | length' "$REPORT_FILE")
unknown=$(jq '[.vulnerabilities[] | select(.severity == "Unknown")] | length' "$REPORT_FILE")

# write summary
if [ "$vulnerabilities" -eq 0 ]; then
  summary="no vulnerablities found :white_check_mark:"
  details=""
else
  summary="$vulnerabilities vulnerabilities found! :x:"
  # details will be non-empty: we add a newline and some indentation
  details="\n     "
fi

# add in details the breakdown by severity
# we only add a severity level if there are some vulnerabilties of that level

if [ "$critical" -gt 0 ]; then
  details="$details :black_medium_square: $critical CRITICAL"
fi

if [ "$high" -gt 0 ]; then
  details="$details :small_red_triangle_down: $high High"
fi

if [ "$medium" -gt 0 ]; then
  details="$details :small_orange_diamond: $medium Medium"
fi

if [ "$low" -gt 0 ]; then
  details="$details :small_blue_diamond: $low Low"
fi

if [ "$info" -gt 0 ]; then
  details="$details :white_medium_square: $info Info"
fi

if [ "$unknown" -gt 0 ]; then
  details="$details :grey_question: $unknown Unknown"
fi

image=$1

# CONTAINER_SCANNING_SLACK_HOOK is defined in Gitlab CI/CD settings
curl -X POST "https://hooks.slack.com/triggers/$CONTAINER_SCANNING_SLACK_HOOK" \
  -H "Content-type: application/json" \
  --data '{
    "image": "'"$image"'",
    "pipeline_url": "'"$CI_PIPELINE_URL"'/security?state=ALL",
    "summary": "'"$summary"'",
    "details": "'"$details"'",
  }'
