# CIAO: CI-in-OCaml

This directory contains an OCaml library for writing generators of
GitLab CI configuration files (i.e. `.gitlab-ci.yml`) and includes a
generator for the tezos/tezos CI configuration.

If you need to update the CI configuration of the
[tezos/tezos](https://gitlab.com/tezos/tezos) project, this is the
directory to investigate.

CIAO and the generator for tezos/tezos is structured like this:

 - `lib_gitlab_ci`: contains a partial, slightly opiniated, AST of
   [GitLab CI/CD YAML syntax](https://docs.gitlab.com/ee/ci/yaml/).
 - `bin`: contains a set of helpers for creating the Octez-specific
   GitLab CI configuration files (`Tezos_ci`, `Rules`) and an
   executable that generates the tezos/tezos CI configuration using
   those helpers.

## The tezos/tezos CI configuration

The tezos/tezos CI configuration consists of a set of *pipeline
types*.  Pipelines of a given type run under a certain *condition* and
perform a given set of automated checks or tasks. For instance:

 - "before merging" pipelines run on merge requests and verify code
   quality.
 - "master branch" pipelines runs after the merge of a merge request
   and publishes bleeding-edge artifacts (documentation, static
   binaries, etc.)

For the full set of pipelines, when they run and what they do, refer
to `bin/main.ml`, where in they are all registered. The jobs of a
given pipeline type is registered in separate modules (for instance,
`master_branch.ml` for master branch pipelines).

## CIAO usage

To regenerate the GitLab CI configuration, run (from the root of the repo):

    make -C ci all

To check that the GitLab CI configuration is up-to-date, run (from the root of the repo):

    make -C ci check

To see all available targets, run:

    make -C ci help

To call the underlying generator binary, call

    dune exec bin/ci/main.exe

If you do not have an OCaml environment installed, then you can
regenerate the configuration through Docker, with the following
command:

    make -C ci docker-all

This will work with all the other targets, such that `make -C ci
docker-TGT` runs `make -C ci TGT` inside a Docker image with the
required prerequisites installed.

For more information, see

    dune exec bin/ci/main.exe -- --help

## Inline documentation

The set of registered pipelines types, and their documentation, can be
accessed by:

    make -s -C ci list-pipelines
