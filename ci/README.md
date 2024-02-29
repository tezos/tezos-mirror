# CI-in-OCaml

This directory contains an OCaml library for writing generators of
GitLab CI configuration files (i.e. `.gitlab-ci.yml`).

This directory is structured like this:

 - `lib_gitlab_ci`: contains a partial, slightly opiniated, AST of
   [GitLab CI/CD YAML syntax](https://docs.gitlab.com/ee/ci/yaml/).
 - `bin`: contains a set of helpers for creating the Octez-specific
   GitLab CI configuration files and an executable that generates part
   of the CI configuration using those helpers.

## Usage

To regenerate the GitLab CI configuration, run (from the root of the repo):

    make -C ci all

To check that the GitLab CI configuration is up-to-date, run (from the root of the repo):

    make -C ci check

To see all available targets, run:

    make -C ci help

To call the underlying generator binary, call

    dune exec bin/ci/main.exe

For more information, see

    dune exec bin/ci/main.exe -- --help
