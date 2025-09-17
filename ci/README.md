# CI Configuration Generator

This directory contains an OCaml library for writing generators of
GitLab CI configuration files (i.e. `.gitlab-ci.yml`) and includes a
generator for the tezos/tezos CI configuration.

If you need to update the CI configuration of the
[tezos/tezos](https://gitlab.com/tezos/tezos) project, this is the
directory to investigate.

## CIAO (CI-in-OCaml)

CIAO is the set of the following subdirectories.

- `lib_gitlab_ci` contains a partial, slightly opinionated, AST of
  [GitLab CI/CD YAML syntax](https://docs.gitlab.com/ee/ci/yaml/).
  It could be used in other, non-Tezos related repositories.

- `lib_tezos_ci` contains a very strongly opinionated wrapper over `lib_gitlab_ci`.
  While using `lib_gitlab_ci` guarantees valid syntax of the generated YAML files,
  `lib_tezos_ci` guarantees some semantics properties.
  For instance, it prevents adding a job to a pipeline if one of its dependencies
  is not also present in this pipeline.
  `lib_tezos_ci` also defines many helpers that only make sense for this repository.

- `lib_tezos_ci_jobs` contains some job definitions for this repository.
  Defining common jobs in this library allows to refer to them from other libraries,
  which in turn allows to split the CI into multiple libraries located at different
  places (such as one library per component).

- `bin`: contains the main CIAO executable which generates the GitLab CI configuration files.

## Cacio

Cacio is another library on top of CIAO which is even more opinionated.
It is located in `lib_cacio`.
It aims to replace CIAO as the interface for developers to define their CI jobs and pipelines.

Compared to CIAO, Cacio adds even more automatic transformations and checks.
It also provides a simplified interface, leaving less choice to the user
when it should not be the role of the user to make those choices.
It is also designed to make it easy to define the CI component by component.
For this to work well, all jobs need to be migrated to Cacio.

Once all jobs are migrated to Cacio, we can remove `lib_tezos_ci_jobs`.
And we can probably start thinking about merging Cacio back into CIAO
(or to reimplement the transformations and checks from `lib_tezos_ci` into Cacio)
to have only one API.

## The tezos/tezos CI configuration

The tezos/tezos CI configuration consists of a set of *pipeline
types*.  Pipelines of a given type run under a certain *condition* and
perform a given set of automated checks or tasks. For instance:

- "before merging" pipelines run on merge requests and verify code quality.

- "merge train" pipelines occur when a merge request is added to a merge train.
  They should not differ much from "before merging" pipelines.

- "master branch" pipelines runs after the merge of a merge request
  and publishes bleeding-edge artifacts (static binaries, etc.)

For the full set of pipelines, when they run and what they do, refer
to `bin/main.ml`, where in they are all registered. The jobs of a
given pipeline type are registered in separate modules in `lib_tezos_ci_jobs`
(for instance, `master_branch.ml` for master branch pipelines).

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

or, if you prefer a more compact view:

    make -s -C ci overview-pipelines

Additionally, you can list the jobs of a given pipeline and their description
with the following command (replace `before_merging` with the pipeline of your choice):

    make -s -C ci before_merging.describe-pipeline
