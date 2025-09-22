(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

module CI = Cacio.Make (struct
  let name = "etherlink"

  let paths = ["etherlink/**/*"; "src/kernel_sdk/**/*"; "sdk/rust/**/*"]
end)

let job_build_evm_node_static =
  Cacio.parameterize @@ fun arch ->
  CI.job
    ("build_evm_node_static_"
    ^ Tezos_ci.Runner.Arch.show_easy_to_distinguish arch)
    ~__POS__
    ~stage:Test
    ~description:"Build the EVM node (statically linked)."
    ~arch
    ?cpu:(match arch with Amd64 -> Some Very_high | Arm64 -> None)
    ?storage:(match arch with Arm64 -> Some Ramfs | Amd64 -> None)
    ~image:Tezos_ci.Images.CI.build
    ~artifacts:
      (Gitlab_ci.Util.artifacts
         ~name:"evm-binaries"
         ~when_:On_success
         ["octez-evm-*"; "etherlink-*"])
    ~cargo_cache:true
    ~cargo_target_caches:true
    ~sccache:(Cacio.sccache ~cache_size:"2G" ())
    [
      "./scripts/ci/take_ownership.sh";
      ". ./scripts/version.sh";
      "eval $(opam env)";
      "make evm-node-static";
    ]

let register () =
  CI.register_before_merging_jobs
    [
      (Manual, job_build_evm_node_static Amd64);
      (Manual, job_build_evm_node_static Arm64);
    ] ;
  CI.register_scheduled_pipeline
    "daily"
    ~description:"Daily tests to run for Etherlink."
    [
      (Auto, job_build_evm_node_static Amd64);
      (Auto, job_build_evm_node_static Arm64);
    ] ;
  ()
