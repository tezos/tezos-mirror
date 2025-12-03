(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Protocol (CLST)
    Invocation:   dune exec src/proto_alpha/lib_protocol/test/integration/main.exe \
                  -- --file test_clst.ml
    Subject:      CLST contract
*)

open Protocol
open Alpha_context

let register_test ~title =
  Tezt_helpers.register_test_es
    ~__FILE__
    ~file_tags:["clst"]
    ~title:("CLST: " ^ title)

let test_deposit =
  register_test ~title:"Test deposit of a non null amount" @@ fun () ->
  let open Lwt_result_wrap_syntax in
  let* b, sender = Context.init1 () in
  let amount = Tez.of_mutez_exn 100000000L in
  let* deposit_tx = Op.clst_deposit (Context.B b) sender amount in
  let* b = Block.bake ~operation:deposit_tx b in
  let* balance =
    Plugin.Contract_services.clst_balance Block.rpc_ctxt b sender
  in
  let amount = Tez.to_mutez amount in
  let balance =
    Option.value_f
      ~default:(fun () -> assert false)
      (Script_int.to_int64 balance)
  in
  Check.((amount = balance) int64 ~error_msg:"Expected %L, got %R") ;
  return_unit

let test_deposit_zero =
  register_test ~title:"Test depositing 0 tez amount is forbidden" @@ fun () ->
  let open Lwt_result_wrap_syntax in
  let* b, sender = Context.init1 () in
  let amount = Tez.of_mutez_exn 0L in
  let* deposit_tx = Op.clst_deposit (Context.B b) sender amount in
  let*! b = Block.bake ~operation:deposit_tx b in
  match b with
  | Ok _ ->
      Test.fail "Empty deposits on CLST are forbidden and expected to fail"
  | Error trace -> Error_helpers.expect_clst_empty_deposit ~loc:__LOC__ trace
