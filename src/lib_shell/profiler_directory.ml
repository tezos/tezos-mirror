(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

let profiler_maker data_dir ~name max_lod =
  Tezos_base.Profiler.instance
    Tezos_base_unix.Simple_profiler.auto_write_to_txt_file
    Filename.Infix.((data_dir // name) ^ "_profiling.txt", max_lod)
