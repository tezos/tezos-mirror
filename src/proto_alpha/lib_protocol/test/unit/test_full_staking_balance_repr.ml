(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    protocol
    Invocation:   dune exec src/proto_alpha/lib_protocol/test/unit/main.exe \
                  -- --file test_full_staking_balance_repr.ml
    Subject:      test the Full_staking_balance_repr module
*)

open Protocol
open Data_encoding

(* We duplicate the definition of the Full_staking_balance_repr module
   from Oxford to test a None case in the current encoding *)
module Full_staking_balance_repr_oxford = struct
  type t = {
    own_frozen : Tez_repr.t;
    staked_frozen : Tez_repr.t;
    delegated : Tez_repr.t;
  }

  let make ~own_frozen ~staked_frozen ~delegated =
    {own_frozen; staked_frozen; delegated}

  let encoding =
    let open Data_encoding in
    conv
      (fun {own_frozen; staked_frozen; delegated} ->
        (own_frozen, staked_frozen, delegated))
      (fun (own_frozen, staked_frozen, delegated) ->
        {own_frozen; staked_frozen; delegated})
      (obj3
         (req "own_frozen" Tez_repr.encoding)
         (req "staked_frozen" Tez_repr.encoding)
         (req "delegated" Tez_repr.encoding))
end

let equal_full_staking_balance (a : Full_staking_balance_repr.t)
    (b : Full_staking_balance_repr.t) =
  let open Lwt_result_syntax in
  let open Full_staking_balance_repr in
  let open Full_staking_balance_repr.Internal_for_tests in
  let equal_tez_repr ~loc a b =
    Assert.equal_int64 ~loc (Tez_repr.to_mutez a) (Tez_repr.to_mutez b)
  in
  let equal_cycle_repr ~loc a b =
    Assert.equal_int32 ~loc (Cycle_repr.to_int32 a) (Cycle_repr.to_int32 b)
  in

  let* () = equal_tez_repr ~loc:__LOC__ (own_frozen a) (own_frozen b) in
  let* () = equal_tez_repr ~loc:__LOC__ (staked_frozen a) (staked_frozen b) in
  let* () =
    equal_tez_repr ~loc:__LOC__ (current_delegated a) (current_delegated b)
  in
  let* () =
    equal_tez_repr
      ~loc:__LOC__
      (min_delegated_in_cycle a)
      (min_delegated_in_cycle b)
  in
  let* () =
    equal_cycle_repr
      ~loc:__LOC__
      (cycle_of_min_delegated a)
      (cycle_of_min_delegated b)
  in
  return_unit

let equal_full_staking_balance_bytes sb_bytes
    (sb_expected : Full_staking_balance_repr.t) =
  let open Lwt_result_syntax in
  let encoding = Full_staking_balance_repr.encoding in
  let* sb =
    match Binary.of_bytes encoding sb_bytes with
    | Ok x -> return x
    | Error e ->
        failwith
          "Data_encoding.Binary.read shouldn't have failed with \
           Full_staking_balance_repr.encoding: %a"
          Binary.pp_read_error
          e
  in
  equal_full_staking_balance sb sb_expected

let test_encodings () =
  let open Lwt_result_syntax in
  let own_frozen = Tez_repr.(mul_exn one 1) in
  let staked_frozen = Tez_repr.(mul_exn one 2) in
  let delegated = Tez_repr.(mul_exn one 5) in

  (* Test a [Some] case for [added_in_p] *)
  let staking_balance =
    Full_staking_balance_repr.init
      ~own_frozen
      ~staked_frozen
      ~delegated
      ~current_cycle:Cycle_repr.root
  in
  let encoding = Full_staking_balance_repr.encoding in
  let sb_bytes = Binary.to_bytes_exn encoding staking_balance in
  let* () = equal_full_staking_balance_bytes sb_bytes staking_balance in

  (* Test a [None] case for [added_in_p] *)
  let staking_balance_o =
    Full_staking_balance_repr_oxford.make ~own_frozen ~staked_frozen ~delegated
  in
  let encoding_o = Full_staking_balance_repr_oxford.encoding in
  let sb_o_bytes = Binary.to_bytes_exn encoding_o staking_balance_o in
  let* () = equal_full_staking_balance_bytes sb_o_bytes staking_balance in
  return_unit

let tests =
  Tztest.[tztest "full staking balance - encoding" `Quick test_encodings]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("full_staking_balance", tests)]
  |> Lwt_main.run
