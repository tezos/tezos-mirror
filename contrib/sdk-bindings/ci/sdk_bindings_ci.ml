(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

open Tezos_ci
open Tezos_ci.Cache

module Files = struct
  let image =
    [
      "images/rust-sdk-bindings/**/*";
      "images/create_image.sh";
      "images/scripts/install_datadog_static.sh";
      "scripts/version.sh";
    ]

  let code = ["sdk/rust/**/*"; "contrib/sdk-bindings/**/*"]

  let all = image @ code
end

module CI = Cacio.Make (struct
  let name = "sdk_bindings"

  let paths = Files.all
end)

(** Set of SDK-bindings related files *)
let changeset = Changeset.make Files.all

let job_test =
  CI.job
    "test"
    ~__POS__
    ~description:"Tests bindings of the Rust SDK"
    ~image:Images.rust_sdk_bindings
    ~stage:Test
    ~allow_failure:Yes
    ~cargo_cache:true
    ~sccache:(Cacio.sccache ~policy:Pull_push ())
    [
      ". $HOME/.venv/bin/activate";
      "make -C contrib/sdk-bindings check";
      "make -C contrib/sdk-bindings test";
    ]

let () =
  CI.register_merge_request_jobs [(Auto, job_test)] ;
  (* TODO: split into a new pipeline [sdk_bindings.daily] *)
  Cacio.Shared.register_schedule_extended_test_jobs [(Auto, job_test)] ;
  ()

module Release = struct
  (** Jobs and pipelines to release SDK bindings for each supported language *)

  let macos_variables : Gitlab_ci.Types.variables =
    [("TAGS", "saas-macos-medium-m1")]

  let windows_variables : Gitlab_ci.Types.variables =
    [("TAGS", "saas-windows-medium-amd64"); ("SHELL", "powershell")]

  let job_check_matching_tag : tezos_job =
    job
      ~__POS__
      ~name:"check_sdk_version"
      ~description:
        "Check that the tag match the `tezos-bindings` Rust package version"
      ~image:Images.datadog_ci
      ~stage:Stages.start
      [
        "CI_MERGE_REQUEST_IID=${CI_MERGE_REQUEST_IID:-none}";
        "DATADOG_SITE=datadoghq.eu datadog-ci tag --level pipeline --tags \
         pipeline_type:$PIPELINE_TYPE --tags mr_number:$CI_MERGE_REQUEST_IID";
        "./contrib/sdk-bindings/scripts/ci/check_tag_version.sh";
      ]

  let jobs_build_sdk =
    let job =
      job
        ~stage:Stages.build
        ~dependencies:(Dependent [Job job_check_matching_tag])
    in

    let build_python_sdk =
      let artifacts =
        Gitlab_ci.Util.artifacts ["contrib/sdk-bindings/rust/target/wheels/*"]
      in

      let linux : tezos_job =
        job
          ~__POS__
          ~name:"build_python_sdk_linux"
          ~description:"Build Python SDK for Linux"
          ~image:Images.rust_sdk_bindings
          ~artifacts
          ~before_script:
            ["export CARGO_NET_OFFLINE=false"; ". $HOME/.venv/bin/activate"]
          ["make -C contrib/sdk-bindings/rust -f python.mk build"]
        |> enable_cargo_cache |> enable_sccache
      in

      let macos : tezos_job =
        job
          ~__POS__
          ~name:"build_python_sdk_macos"
          ~description:"Build Python SDK on macOS"
          ~image:Images.macosx_15
          ~variables:macos_variables
          ~tag:Dynamic
          ~artifacts
          ~before_script:
            [
              "export CARGO_NET_OFFLINE=false";
              "export CARGO_HOME=$HOME/.cargo";
              "python3 -m venv $HOME/.venv";
              ". $HOME/.venv/bin/activate";
              "curl https://sh.rustup.rs -sSf | sh -s -- -y";
              ". $HOME/.cargo/env";
              "pip install maturin==1.5.1";
              "brew install sccache";
            ]
          ["make -C contrib/sdk-bindings/rust -f python.mk build"]
        |> enable_cargo_cache |> enable_sccache
      in

      let windows : tezos_job =
        job
          ~__POS__
          ~name:"build_python_sdk_windows"
          ~description:"Build Python SDK on Windows"
          ~variables:windows_variables
          ~tag:Dynamic
          ~datadog:false
          ~artifacts
          ~before_script:
            [
              (* Install Rust *)
              "[Environment]::SetEnvironmentVariable('CARGO_NET_OFFLINE','false')";
              "[Environment]::SetEnvironmentVariable('CARGO_HOME','.cargo')";
              {|$env:Path = "$Env:CI_PROJECT_DIR\.cargo\bin;$env:Path"|};
              "Invoke-WebRequest -Uri https://win.rustup.rs -OutFile \
               rustup-init.exe";
              "./rustup-init.exe -y";
              "Remove-Item rustup-init.exe";
              (* Install Maturin *)
              "pip install maturin==1.5.1";
              (* Install make *)
              "choco install make";
            ]
          [
            "make -C $Env:CI_PROJECT_DIR/contrib/sdk-bindings/rust -f \
             python.mk build";
          ]
      in

      [linux; macos; windows]
    in
    build_python_sdk

  let job_publish_sdk =
    let maturin_variables : Gitlab_ci.Types.variables =
      [
        ("MATURIN_REPOSITORY", "testpypi");
        ("MATURIN_PYPI_TOKEN", "$CI_TESTPYPI_TOKEN");
      ]
    in
    let dependencies =
      Dependent (List.map (fun job -> Artifacts job) jobs_build_sdk)
    in
    job
      ~__POS__
      ~name:"publish_sdk"
      ~description:"Publish all previously built SDK"
      ~stage:Stages.publish
      ~image:Images.rust_sdk_bindings
      ~variables:maturin_variables
      ~dependencies
      ~before_script:[". $HOME/.venv/bin/activate"]
      ["make -C contrib/sdk-bindings publish"]

  let jobs = [job_check_matching_tag] @ jobs_build_sdk @ [job_publish_sdk]

  (* Matches Tezos SDK release tags, e.g. [tezos-sdk-v1.2.0]. *)
  let tag_re = "/^tezos-sdk-v\\d+\\.\\d+\\.\\d+$/"

  let () =
    let open Gitlab_ci in
    let open Rules in
    Pipeline.register
      "publish_sdk_bindings_releases"
      If.(on_tezos_namespace && push && has_tag_match tag_re)
      ~jobs
      ~description:
        "Release tag pipeline for SDK-bindings.\n\n\
         Created when the release manager pushes a tag in the format \
         tezos-sdk-vX.Y.Z. Creates and publishes releases on each supported \
         package manager. Supported package managers are: 'TestPyPI'"
end
