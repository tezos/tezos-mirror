(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

module CI = Cacio.Make (struct
  let name = "client-libs"

  let paths =
    [
      "images/client-libs-dependencies/**/*";
      "client-libs/*kaitai*/**/*";
      (* Encodings are defined all over the place,
         and Kaitai files depends on encodings. *)
      "src/**/*";
    ]
end)

(* Note: this is both a build job and a test job.
   It may be more clear to split it into:
   - one job to build codec.exe;
   - one job to test that codec.exe produces the same Kaitai files
     as the ones that are committed.
   The latter could be merged with kaitai_e2e_checks, or not.
   This refactoring would however require reworking the make target. *)
let job_kaitai_checks =
  CI.job
    "kaitai_checks"
    ~__POS__
    ~description:
      "Build the Kaitai files (.ksy) from Octez encodings, and check that they \
       are the same as the ones that are committed in the repository. Also \
       build bin_codec_kaitai/codec.exe and store it as an artifact."
    ~image:Tezos_ci.Images.CI.build
    ~stage:Test
    ~artifacts:
      (Gitlab_ci.Util.artifacts
         ~expire_in:(Duration (Hours 1))
         ~when_:On_success
         ["_build/default/client-libs/bin_codec_kaitai/codec.exe"])
    ~cargo_cache:true
    ~sccache:(Cacio.sccache ())
    [
      ". ./scripts/version.sh";
      "eval $(opam env)";
      "make -C ${CI_PROJECT_DIR} check-kaitai-struct-files || (echo 'Octez \
       encodings and Kaitai files seem to be out of sync. You might need to \
       run `make check-kaitai-struct-files` and commit the resulting diff.' ; \
       false)";
    ]

let job_kaitai_e2e_checks =
  CI.job
    "kaitai_e2e_checks"
    ~__POS__
    ~description:"Run the end-to-end Kaitai tests."
    ~image:Tezos_ci.Images.client_libs_dependencies
    ~stage:Test
    ~needs:[(Artifacts, job_kaitai_checks)]
    [
      ". ./scripts/version.sh";
      (* TODO: https://gitlab.com/tezos/tezos/-/issues/5026
         As observed for the `unit:js_components` running `npm i`
         every time we run a job is inefficient.

         The benefit of this approach is that we specify node version
         and npm dependencies (package.json) in one place, and that the local
         environment is then the same as CI environment. *)
      ". ./scripts/install_build_deps.js.sh";
      "./client-libs/kaitai-struct-files/scripts/kaitai_e2e.sh \
       client-libs/kaitai-struct-files/files 2>/dev/null";
    ]

let register () =
  CI.register_merge_request_jobs
    [(Manual, job_kaitai_checks); (Manual, job_kaitai_e2e_checks)] ;
  CI.register_scheduled_pipeline
    "daily"
    ~description:"Daily tests to run for client-libs."
    [(Auto, job_kaitai_checks); (Auto, job_kaitai_e2e_checks)] ;
  ()
