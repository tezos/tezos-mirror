(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------
   Component:    Dal node storage
   Invocation:   dune exec src/lib_dal_node/test/main.exe \
                  -- --file test_storage.ml
   Subject:      Dummy test
*)

let dummy_test =
  let open Lwt_result_syntax in
  let property _ =
    let run = return_unit in
    match Lwt_main.run run with
    | Ok () -> Ok ()
    | Error err ->
        Error (`Fail (Format.asprintf "%a" Error_monad.pp_print_trace err))
  in
  Tezt_bam.Pbt.register
    ~pp:Format.pp_print_bool
    ~hash:Ppx_hash_lib.Std.Hash.Builtin.hash_bool
    ~minimum_number_of_samples:10
    ~title:"dummy test"
    ~stop_after:(`Timeout 30.)
    ~__FILE__
    ~tags:["kvs"; "sqlite"]
    ~gen:(Bam.Std.bool ())
    ~property
    ()
