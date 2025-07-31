(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

module CI = Cacio.Make (struct
  let name = "documentation"

  let paths = ["**/*.rst"]
end)

let job_rst_check =
  CI.job
    "rst-check"
    ~__POS__
    ~image:Tezos_ci.Images.CI.test_master
    ~stage:Test
    ~description:"Check ReStructured Text files."
    [". $HOME/.venv/bin/activate"; "make --silent -C docs sphinx-check"]

let register () =
  CI.register_before_merging_jobs [(Immediate, job_rst_check)] ;
  CI.register_scheduled_pipeline
    "daily"
    ~description:"Daily tests to run for the documentation."
    [(Auto, job_rst_check)] ;
  ()
