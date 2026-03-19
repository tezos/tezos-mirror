(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

open Tezos_ci

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
  Cacio.register_merge_request_jobs [(Auto, job_test)] ;
  (* TODO: split into a new pipeline [sdk_bindings.daily] *)
  Cacio.register_jobs Schedule_extended_test [(Auto, job_test)] ;
  ()

module Release = struct
  (** Jobs and pipelines to release SDK bindings for each supported language *)

  let macos_variables : Gitlab_ci.Types.variables =
    [("TAGS", "saas-macos-medium-m1")]

  let windows_variables : Gitlab_ci.Types.variables =
    [("TAGS", "saas-windows-medium-amd64"); ("SHELL", "powershell")]

  let job_check_version =
    CI.job
      "check_version"
      ~__POS__
      ~description:
        "Check that the tag match the `tezos-bindings` Rust package version"
      ~image:Images.datadog_ci
      ~stage:Test
      [
        "CI_MERGE_REQUEST_IID=${CI_MERGE_REQUEST_IID:-none}";
        "DATADOG_SITE=datadoghq.eu datadog-ci tag --level pipeline --tags \
         pipeline_type:$PIPELINE_TYPE --tags mr_number:$CI_MERGE_REQUEST_IID";
        "./contrib/sdk-bindings/scripts/ci/check_tag_version.sh";
      ]

  let artifacts =
    Gitlab_ci.Util.artifacts ["contrib/sdk-bindings/rust/target/wheels/*"]

  let job_build_python_linux =
    CI.job
      "build_python_linux"
      ~__POS__
      ~description:"Build Python SDK for Linux"
      ~stage:Build
      ~image:Images.rust_sdk_bindings
      ~artifacts
      ~cargo_cache:true
      ~sccache:(Cacio.sccache ())
      [
        "export CARGO_NET_OFFLINE=false";
        ". $HOME/.venv/bin/activate";
        "make -C contrib/sdk-bindings/rust -f python.mk build";
      ]

  let job_build_python_macos =
    CI.job
      "build_python_macos"
      ~__POS__
      ~description:"Build Python SDK on macOS"
      ~stage:Build
      ~image:Images.macosx_15
      ~variables:macos_variables
      ~tag:Dynamic
      ~artifacts
      ~cargo_cache:true
      ~sccache:(Cacio.sccache ())
      [
        (* Prepare *)
        "export CARGO_NET_OFFLINE=false";
        "export CARGO_HOME=$HOME/.cargo";
        "python3 -m venv $HOME/.venv";
        ". $HOME/.venv/bin/activate";
        "curl https://sh.rustup.rs -sSf | sh -s -- -y";
        ". $HOME/.cargo/env";
        "pip install maturin==1.5.1";
        "brew install sccache";
        (* Build *)
        "make -C contrib/sdk-bindings/rust -f python.mk build";
      ]

  let job_build_python_windows =
    CI.job
      "build_python_windows"
      ~__POS__
      ~description:"Build Python SDK on Windows"
      ~stage:Build
      ~variables:windows_variables
      ~tag:Dynamic
      ~disable_datadog:true
      ~artifacts
      [
        (* Install Rust *)
        "[Environment]::SetEnvironmentVariable('CARGO_NET_OFFLINE','false')";
        "[Environment]::SetEnvironmentVariable('CARGO_HOME','.cargo')";
        {|$env:Path = "$Env:CI_PROJECT_DIR\.cargo\bin;$env:Path"|};
        "Invoke-WebRequest -Uri https://win.rustup.rs -OutFile rustup-init.exe";
        "./rustup-init.exe -y";
        "Remove-Item rustup-init.exe";
        (* Install Maturin *)
        "pip install maturin==1.5.1";
        (* Install make *)
        "choco install make";
        (* Build *)
        "make -C $Env:CI_PROJECT_DIR/contrib/sdk-bindings/rust -f python.mk \
         build";
      ]

  let job_publish_sdk =
    CI.job
      "publish"
      ~__POS__
      ~description:"Publish all previously built SDK"
      ~stage:Publish
      ~image:Images.rust_sdk_bindings
      ~variables:
        [
          ("MATURIN_REPOSITORY", "testpypi");
          ("MATURIN_PYPI_TOKEN", "$CI_TESTPYPI_TOKEN");
        ]
      ~needs:
        [
          (Job, job_check_version);
          (Artifacts, job_build_python_linux);
          (Artifacts, job_build_python_macos);
          (Artifacts, job_build_python_windows);
        ]
      [". $HOME/.venv/bin/activate"; "make -C contrib/sdk-bindings publish"]

  let () =
    (* [~tag_rex] matches Tezos SDK release tags, e.g. [tezos-sdk-v1.2.0]. *)
    CI.register_dedicated_release_pipeline
      ~tag_rex:"/^tezos-sdk-v\\d+\\.\\d+\\.\\d+$/"
      [(Auto, job_publish_sdk)]
end
