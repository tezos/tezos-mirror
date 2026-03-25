(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs. <contact@nomadic-labs.com>               *)
(* Copyright (c) 2024 TriliTech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(* This module contains the definition of stages and Docker
   images used by the Octez CI pipelines.

   It also defines:
    - helpers for defining jobs;
    - changesets shared by jobs;
    - helpers for making jobs;
    - jobs shared between pipelines *)

open Gitlab_ci.Types
open Tezos_ci

(** {2 Shared Helpers} *)

module Helpers = struct
  (** The default [before_script:] section.

    In general, the result of this script should be used as the
    default value for [~before_merging] for all jobs. Each boolean flag
    of this function enables a specific functionality before the job's
    [script:] runs. In detail:

    - [take_ownership]: all files in the working directory of the
      job are [chown]'d by the job's user. This requires that either
      sudo is installed in the job's image or that the job's user has
      sufficient privileges. (default: [false])
    - [source_version]: the script [scripts/version.sh] is sourced. (default: [false])
    - [eval_opam]: runs [eval $(opam env)], activating any opam switch
      if present in the image. (default: [false])
    - [init_python_venv]: runs [.venv/bin/activate], activating any
      python vinv if present in the image. (default: [false])
    - [install_js_deps]: runs, and sources,
      [./scripts/install_build_deps.js.sh] installing JavaScript
      dependencies and [node], [nvm] and [npm] available in the
      environment. (default: [false])

   The unnamed argument of the function is appended to the end of the
   [before_script:] section, after any of the additions caused by the
   optional arguments. *)
  let before_script ?(take_ownership = false) ?(source_version = false)
      ?(eval_opam = false) ?(init_python_venv = false)
      ?(install_js_deps = false) before_script =
    let toggle t x = if t then [x] else [] in
    (* FIXME: https://gitlab.com/tezos/tezos/-/issues/2865 *)
    toggle take_ownership "./scripts/ci/take_ownership.sh"
    @ toggle source_version ". ./scripts/version.sh"
      (* TODO: this must run in the before_script of all jobs that use the opam environment.
       how to enforce? *)
    @ toggle eval_opam "eval $(opam env)"
    (* Load the environment poetry previously created in the docker image.
     Give access to the Python dependencies/executables *)
    @ toggle init_python_venv ". $HOME/.venv/bin/activate"
    @ toggle install_js_deps ". ./scripts/install_build_deps.js.sh"
    @ before_script
end

(** {2 Shared Docker jobs} *)

module Docker = struct
  (** Type of Docker build jobs.

    The semantics of the type is summed up in this table:

    |                       | Release    | Experimental | Test   | Test_manual |
    |-----------------------+------------+--------------+--------+-------------|
    | Image registry        | Docker hub | Docker hub   | GitLab | GitLab      |
    | Experimental binaries | no         | yes          | yes    | yes         |
    | EVM Kernels           | no         | On amd64     | no     | On amd64    |
    | Manual job            | no         | no           | no     | yes         |

    - [Release] Docker builds include only released executables whereas other
      types also includes experimental ones.
    - [Test_manual] and [Experimental] Docker builds include the EVM kernels in
      amd64 builds.
    - [Release] and [Experimental] Docker builds are pushed to Docker hub,
      whereas other types are pushed to the GitLab registry.
    - [Test_manual] Docker builds are started manually, put in the stage
      [manual] and their failure is allowed. The other types are in the build
      stage, run [on_success] and are not allowed to fail. *)
  type docker_build_type = Experimental | Release | Test | Test_manual

  (** Creates a Docker build job of the given [arch] and [docker_build_type]. *)
  let job_docker_build ?rules ?dependencies ~__POS__ ~arch ?storage
      docker_build_type : tezos_job =
    let arch_string = Runner.Arch.show_uniform arch in
    let ci_docker_hub =
      match docker_build_type with
      | Release | Experimental -> true
      | Test | Test_manual -> false
    in
    (* Whether to include evm artifacts.
     Including these artifacts requires the rust-toolchain image. *)
    let with_evm_artifacts =
      match (arch, docker_build_type) with
      | Amd64, (Test_manual | Experimental) -> true
      | _ -> false
    in
    let image_dependencies =
      (* TODO: https://gitlab.com/tezos/tezos/-/issues/7293

       In reality, we actually require both
       {!Images.CI.runtime} and
       {!Images.CI.build}. But these two images are
       created by the same job, and depending on them both will create
       a duplicated dependency on that single job, which GitLab CI
       does not allow. This should be somehow handled by CIAO. *)
      [Images.CI.runtime]
      @ if with_evm_artifacts then [Images.rust_toolchain] else []
    in
    let variables =
      [
        ( "DOCKER_BUILD_TARGET",
          if with_evm_artifacts then "with-evm-artifacts"
          else "without-evm-artifacts" );
        ("IMAGE_ARCH_PREFIX", arch_string ^ "_");
        ( "EXECUTABLE_FILES",
          match docker_build_type with
          | Release -> "script-inputs/released-executables"
          | Test | Test_manual | Experimental ->
              "script-inputs/released-executables \
               script-inputs/experimental-executables" );
      ]
    in
    let stage =
      match docker_build_type with
      | Test_manual -> Stages.manual
      | _ -> Stages.build
    in
    let retry =
      match docker_build_type with
      | Test_manual -> None
      | _ ->
          (* Set retry to 1 because the job is a bit flaky.
             The runner sometimes dies, causing the job to fail with EOF.
             Perhaps surprisingly, this surfaces as a [Script_failure]. *)
          Some
            {
              Gitlab_ci.Types.max = 1;
              when_ = [Script_failure; Runner_system_failure];
            }
    in
    let name = "oc.docker:" ^ arch_string in
    job_docker_authenticated
      ?rules
      ?dependencies
      ~image_dependencies
      ~ci_docker_hub
      ?retry
      ~__POS__
      ~stage
      ~arch
      ?storage
      ~name
      ~variables
      ["./scripts/ci/docker_release.sh"]
end

(** {2 Helpers for Debian/RPM packaging jobs} *)

module Packaging = struct
  (* types for the repositories pipelines.
   - Release: we run all the release jobs, but no tests
   - Partial: we run only a subset of the tests jobs
   - Full: we run the complete test matrix
*)
  type repository_pipeline = Full | Partial | Release

  (** Return a tuple (ARCHITECTURES, <archs>) based on the type
    of repository pipeline. *)
  let archs_variables pipeline =
    let archs : Runner.Arch.t list =
      match pipeline with
      | Partial -> [Amd64]
      | Full | Release -> [Amd64; Arm64]
    in
    [
      ( "ARCHITECTURES",
        String.concat " " (List.map Runner.Arch.show_uniform archs) );
    ]

  let make_variables ?(kind = "build") add =
    ( "DEP_IMAGE",
      sf "${GCP_REGISTRY}/$CI_PROJECT_NAMESPACE/tezos/%s-$DISTRIBUTION" kind )
    (* this second variable is for a read only registry and we want it to be
            tezos/tezos *)
    :: ( "DEP_IMAGE_PROTECTED",
         sf "${GCP_PROTECTED_REGISTRY}/tezos/tezos/%s-$DISTRIBUTION" kind )
    :: add

  let make_docker_build_dependencies ~__POS__ ?rules ~name ~matrix ~distribution
      ~base_image ~script () =
    job_docker_authenticated
      ~__POS__
      ~name
      ?rules
      ~stage:Stages.images
      ~variables:
        (make_variables
           [("DISTRIBUTION", distribution); ("BASE_IMAGE", base_image)])
      ~parallel:(Matrix matrix)
      ~tag:Dynamic
      script

  let make_job_merge_build_dependencies ~distribution ~dependencies ~matrix =
    job_docker_authenticated
      ~__POS__
      ~name:(Format.sprintf "oc.docker-build-merge-manifest.%s" distribution)
      ~stage:Stages.images
      ~dependencies
      ~variables:
        (make_variables
           [("DISTRIBUTION", distribution); ("IMAGE_NAME", "$DEP_IMAGE")])
      ~parallel:(Matrix matrix)
      ["scripts/ci/docker-merge-base-images.sh"]
end
