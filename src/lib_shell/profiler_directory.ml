(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

let profiler_maker data_dir ~name max_verbosity =
  Tezos_base_unix.Simple_profiler.instantiate_default_driver
    Filename.Infix.
      ((data_dir // name) ^ Profiler.profiler_file_suffix, max_verbosity)
