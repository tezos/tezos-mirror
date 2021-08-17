#!/usr/bin/env bash

set -eu

TZ_OPAM_FILES_MODIFIED="${TZ_OPAM_FILES_MODIFIED:-false}"

# shellcheck disable=SC2016
# We intentionally use single quotes to not allow variable substitution inside the jq query.
yq -y -n \
   --arg pkgs "$PACKAGES" \
   --arg modified_flag "$TZ_OPAM_FILES_MODIFIED" \
   '( $pkgs | split(" ")) | map(
{
  "opam:\(.)": {
    extends: [
      ".default_settings_template",
      ".image_template__runtime_build_test_dependencies_template",
      ".rules_template__development_opam"
    ],
    stage: "packaging",
    variables: {
      "TZ_OPAM_FILES_MODIFIED": $modified_flag
    },
    script: [
      "./scripts/opam-pin.sh",
      "opam depext --yes \(.)",
      "opam install --yes \(.)",
      "opam reinstall --yes --with-test \(.)"
    ]
  }
}
) | add | . + {
  include: ".gitlab/ci/templates.yml",
  stages: [ "packaging" ],
  "opam:pipeline_init": {
    stage: "packaging",
    script: "echo Pipeline generated"
  }
}'
