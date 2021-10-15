#!/bin/sh

# This script is called in the opam tests of the Gitlab CI. It
# collects the outputs of the opam builds, puts them in the folder
# opam_logs for storage as artifacts. Additionally, it merges the
# logs to a single file HTML for easier browsing. Finally, it outputs
# the first 1000 lines of the merged output.

OPAM_LOGS=${OPAM_LOGS:-opam_logs}
OPAM_DIR="$HOME"/.opam

# artifacts:paths requires a relative path. to avoid calculating the
# relative path, we use rsync to move all outputs to a sub-directory
# of the current working directory, as specified by the environment
# variable OPAM_LOGS, defaulting to opam_logs.
mkdir -p "$OPAM_LOGS"

rsync --recursive --prune-empty-dirs \
      "$OPAM_SWITCH_PREFIX"/.opam-switch/build \
      --include="*/" \
      --include '*.output' \
      --exclude '*' \
      "$OPAM_LOGS/output"

rsync --recursive "$OPAM_DIR"/log \
      --include "*/" \
      --include '*.info' \
      --exclude '*' "$OPAM_LOGS"

 # for ease of readability, produce a merged log
merged=$(mktemp);

IFS=$(printf '\n') \
   find "$OPAM_LOGS" -type f |
    # Sort files by modification time
    while read -r file; do
        printf '%d %s\n' "$(stat -c +%Y "$file")" "$file"
    done | sort -k1nr | cut -f 2- -d ' ' |
    while read -r file; do
        echo "------------------- $file -------------------"
        cat "$file"
    done > "$merged"

merged_html="$OPAM_LOGS"/merged_output.html
(
    echo "<html><body><pre>"
    cat "$merged"
    echo "</pre></body></html>"
) > "${merged_html}"

if [ "$CI_JOB_STATUS" != 'success' ]; then
    merged_output_url=$CI_SERVER_URL/$CI_PROJECT_NAMESPACE/$CI_PROJECT_NAME/-/jobs/$CI_JOB_ID/artifacts/file/"$OPAM_LOGS"/merged_output.html
    echo "-- Job was non-successful (job status: ${CI_JOB_STATUS:-N/A}), merged output:"
    echo ""
    cutoff=1000
    head -n $cutoff "$merged"
    line_count=$(wc -l "$merged" | cut -d' ' -f1)
    if [ "$line_count" -gt $cutoff ]; then
        echo "... see artifacts for full output at $merged_output_url";
    fi
fi
