(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Functori, <contact@functori.com>             *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs, <contact@nomadic-labs.com>     *)
(*                                                                           *)
(*****************************************************************************)

let last_processed_level_filename = "last_processed_level"

module KVS = Key_value_store

type nonrec t = (unit, unit, int32) KVS.t

let init ~root_dir : t tzresult Lwt.t = KVS.init ~lru_size:1 ~root_dir

let file_layout ~root_dir () =
  let filepath = Filename.concat root_dir last_processed_level_filename in
  Key_value_store.layout
    ~encoding:Data_encoding.int32
    ~filepath
    ~eq:Stdlib.( = )
    ~index_of:(fun () -> 0)
    ~number_of_keys_per_file:1
    ()

let load_last_processed_level (t : t) =
  let open Lwt_result_syntax in
  let* exists = KVS.value_exists t file_layout () () in
  if exists then
    let* res = KVS.read_value t file_layout () () in
    return_some res
  else return_none

let save_last_processed_level (t : t) ~level =
  KVS.write_value ~override:true t file_layout () () level
