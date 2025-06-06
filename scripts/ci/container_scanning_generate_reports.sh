#!/bin/sh

set -eu

trivy --version

# update vulnerabilities db
time trivy image --download-db-only

# Update @contrib/gitlab.tpl to manage report aggregation compatibility: unique id given
# Report schema: https://gitlab.com/gitlab-org/security-products/security-report-schemas/-/blob/master/dist/container-scanning-report-format.json
sed -i 's/"id": "{{ .VulnerabilityID }}",/"id": "{{ uuidv4 }}",/g' /contrib/gitlab.tpl

# This template modification will add the package name after the image name to avoid GitLab Deduplication process
# https://docs.gitlab.com/user/application_security/vulnerability_report/pipeline/#deduplication-process
# shellcheck disable=SC2016
sed -i 's/"image": "{{ $image }}"/"image": "{{ $image }}:{{ .PkgName }}"/g' /contrib/gitlab.tpl

# Builds report and puts it in the default workdir $CI_PROJECT_DIR, so `artifacts:` can take it from there
time trivy image --exit-code 0 --format template --template "@/contrib/gitlab.tpl" --output "$CI_PROJECT_DIR/$REPORT" "$FULL_IMAGE_NAME"

# Prints full report
time trivy image --exit-code 0 "$FULL_IMAGE_NAME"
