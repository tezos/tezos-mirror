#!/usr/bin/env bash

set -eu

TZ_OPAM_FILES_MODIFIED="${TZ_OPAM_FILES_MODIFIED:-false}"

cat <<EOF
include: ".gitlab/ci/templates.yml"

stages:
  - packaging

opam:pipeline_init:
  stage: "packaging"
  script: "echo Pipeline generated"
EOF

for PKG in $PACKAGES; do
  cat <<EOF

opam:$PKG:
  extends:
    - .default_settings_template
    - .image_template__runtime_build_test_dependencies_template
    - .rules_template__development_opam
  stage: packaging
  variables:
    TZ_OPAM_FILES_MODIFIED: "$TZ_OPAM_FILES_MODIFIED"
  script:
    - ./scripts/opam-pin.sh
    - opam depext --yes $PKG
    - opam install --yes $PKG
    - opam reinstall --yes --with-test $PKG
EOF
done
