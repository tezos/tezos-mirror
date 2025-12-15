(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(* This file defines jobs that were migrated to Cacio,
   but which are not yet part of a component.

   As such, the jobs are defined in the [Shared] component,
   and are added to the [scheduled_extended_test] pipeline. *)

module Files = struct
  let riscv = ["src/riscv/**/*"; "sdk/rust/**/*"]

  let kernels = "src/kernel_sdk/**/*" :: riscv

  let test_kernels = "kernels.mk" :: "src/kernel_*/**/*" :: riscv

  let rust_toolchain =
    [
      "images/rust-toolchain/**/*";
      "images/create_image.sh";
      "images/scripts/install_datadog_static.sh";
      "scripts/version.sh";
    ]
end

module CI = Cacio.Shared

(* Common configuration for kernel jobs. *)
let job_kernel ?(sccache = Cacio.sccache ()) =
  CI.job
    ~cargo_cache:true
    ~sccache
    ~variables:[("CC", "clang"); ("NATIVE_TARGET", "x86_64-unknown-linux-musl")]

let job_check_riscv_kernels =
  job_kernel
    "check_riscv_kernels"
    ~__POS__
    ~stage:Test
    ~description:"Run 'make check' in 'src/riscv'."
    ~only_if_changed:Files.(kernels @ rust_toolchain)
    ~image:Tezos_ci.Images.rust_toolchain
    [
      (* EXTRA_FLAGS ensure we don't need Ocaml installed in the check and test jobs. *)
      "make -C src/riscv CHECK_FLAGS= EXTRA_FLAGS='--no-default-features \
       --features ci' check";
    ]

let job_audit_riscv_deps =
  job_kernel
    "audit_riscv_deps"
    ~__POS__
    ~stage:Test
    ~description:"Run 'make audit' in 'src/riscv'."
    ~only_if_changed:Files.kernels
      (* Since we depend on the Images.rust_toolchain_master,
         we start the job only if the code is modified, but not the image itself. *)
    ~image:Tezos_ci.Images.rust_toolchain_master
    ["make -C src/riscv audit"]

let job_test_kernels =
  job_kernel
    "test_kernels"
    ~__POS__
    ~stage:Test
    ~description:"Run 'make check' and 'make test' for kernels."
    ~only_if_changed:Files.(test_kernels @ rust_toolchain)
    ~image:Tezos_ci.Images.rust_toolchain
    ["make -f kernels.mk check"; "make -f kernels.mk test"]

let job_build_kernels =
  job_kernel
    "oc.build_kernels"
    ~__POS__
    ~stage:Build
    ~description:"Build the kernels, including the Etherlink kernel."
    ~cpu:Very_high
    ~image:Tezos_ci.Images.rust_toolchain
    ~only_if_changed:
      [
        (* This job is used by other jobs such as [etherlink.test_kernels]
           and is thus automatically added to pipelines that modify the changeset
           of those other jobs. But we also want to check that kernels compile
           if the makefiles are modified, and [kernels.mk] does not trigger those other jobs.
           So we explicitly add the makefiles to the changeset and we explicitly
           register [job_build_kernels] in the pipelines. *)
        "kernels.mk";
        "etherlink.mk";
      ]
    ~artifacts:
      (Gitlab_ci.Util.artifacts
         ~name:"build-kernels-$CI_COMMIT_REF_SLUG"
         ~expire_in:(Duration (Days 1))
         ~when_:On_success
         [
           "evm_kernel.wasm";
           "smart-rollup-installer";
           "sequenced_kernel.wasm";
           "tx_kernel.wasm";
           "tx_kernel_dal.wasm";
           "dal_echo_kernel.wasm";
         ])
    ~sccache:(Cacio.sccache ~policy:Pull_push ())
    ["make -f kernels.mk build"; "make -f etherlink.mk evm_kernel.wasm"]

let register () =
  CI.register_merge_request_jobs
    [
      (Auto, job_check_riscv_kernels);
      (Immediate, job_audit_riscv_deps);
      (Auto, job_test_kernels);
      (Auto, job_build_kernels);
    ] ;
  CI.register_schedule_extended_test_jobs
    [
      (Auto, job_check_riscv_kernels);
      (Auto, job_audit_riscv_deps);
      (Auto, job_test_kernels);
      (Auto, job_build_kernels);
    ] ;
  ()
