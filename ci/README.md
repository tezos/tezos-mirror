# CI-in-OCaml

This directory contains an OCaml library for writing generators of
GitLab CI configuration files (i.e. `.gitlab-ci.yml`).

This directory is structured like this:

 - `lib_gitlab_ci`: contains a partial, slightly opiniated, AST of
   [GitLab CI/CD YAML syntax](https://docs.gitlab.com/ee/ci/yaml/).
