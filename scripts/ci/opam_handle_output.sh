#!/bin/sh

# This script is called in the opam tests of the Gitlab CI. It
# collects the outputs of the opam builds, puts them in the folder
# opam_logs for storage as artifacts. Additionally, it merges the
# logs to a single file HTML for easier browsing. Finally, it outputs
# the first 1000 lines of the merged output.

OPAM_LOGS=${OPAM_LOGS:-opam_logs}

# artifacts:paths requires a relative path. to avoid calculating the
# relative path, we use rsync to move all outputs to a sub-directory
# of the current working directory, as specified by the environment
# variable OPAM_LOGS, defaulting to opam_logs.
rsync --recursive --prune-empty-dirs \
      "$OPAM_SWITCH_PREFIX"/.opam-switch/build \
      --include="*/" \
      --include '*.output' \
      --exclude '*' \
      "$OPAM_LOGS"

 # for ease of readability, produce a merged log
merged=$(mktemp);
(
    find "$OPAM_LOGS/" \
         -iname \*.output \
         -exec echo "------------------- {} -------------------" ";" \
         -exec cat "{}" ";"
) > "$merged"

merged_html="$OPAM_LOGS"/merged_output.html
echo "<html><body><pre>" > "${merged_html}"
cat "$merged" >> "${merged_html}"
echo "</pre></body></html>" >> "${merged_html}"

if [ "$CI_JOB_STATUS" != 'success' ]; then
    echo "-- Job was non-successful (job status: $CI_JOB_STATUS), merged output:"
    echo ""
    cutoff=1000
    head -n $cutoff "$merged"
    line_count=$(wc -l "$merged" | cut -d' ' -f1)
    if [ "$line_count" -gt $cutoff ]; then
        echo "... see artifacts for full output";
    fi
fi
