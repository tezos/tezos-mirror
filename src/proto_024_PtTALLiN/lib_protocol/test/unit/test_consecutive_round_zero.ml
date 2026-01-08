(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    protocol
    Invocation:   dune exec src/proto_024_PtTALLiN/lib_protocol/test/unit/main.exe \
                  -- --file test_consecutive_round_zero.ml
    Subject:      test consecutive_round_zero which corresponds to the number
                  of blocks consecutively baked at round zero.
*)

let register_test =
  Tezt_helpers.register_test_es ~__FILE__ ~file_tags:["consecutive_round_zero"]

(* blocks_per_cycle = 12l *)
let constants =
  {Default_parameters.constants_test with consensus_threshold_size = 0}

let check_consecutive_round_zero ~loc ~title b ~expected =
  let open Lwt_result_syntax in
  let* counter = Context.get_consecutive_round_zero (B b) in
  let* () = Assert.equal_int32 ~loc expected counter in
  Log.info
    ~color:Log.Color.FG.blue
    "%s : consecutive_round_zero = %s"
    title
    (Int32.to_string counter) ;
  return_unit

let () =
  register_test ~title:"some blocks are baked not at round zero" @@ fun () ->
  let open Lwt_result_syntax in
  let* b, _delegate = Context.init_with_constants1 constants in
  let* () =
    check_consecutive_round_zero ~loc:__LOC__ ~title:"initial" b ~expected:0l
  in
  let* b = Block.bake_n 20 b in
  let* () =
    check_consecutive_round_zero
      ~loc:__LOC__
      ~title:"after baking 20 blocks"
      b
      ~expected:20l
  in
  let* b = Block.bake ~policy:(By_round 2) b in
  let* () =
    check_consecutive_round_zero
      ~loc:__LOC__
      ~title:"21st block at round 2"
      b
      ~expected:0l
  in
  let* b = Block.bake_n 5 b in
  let* () =
    check_consecutive_round_zero
      ~loc:__LOC__
      ~title:"after baking 5 blocks"
      b
      ~expected:5l
  in
  return_unit
