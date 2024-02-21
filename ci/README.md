# CI-in-OCaml

This directory contains an OCaml library for writing generators of
GitLab CI configuration files (i.e. `.gitlab-ci.yml`).

This directory is structured like this:

 - `lib_gitlab_ci`: contains a partial, slightly opiniated, AST of
   [GitLab CI/CD YAML syntax](https://docs.gitlab.com/ee/ci/yaml/).
 - `bin`: contains a set of helpers for creating the Octez-specific
   GitLab CI configuration files and the skeleton of an executable that will be used for
   for writing `.gitlab-ci.yml` using those helpers.
