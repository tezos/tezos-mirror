(*****************************************************************************)
(*                                                                           *)
(* MIT License                                                               *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Epoxy_tx.Tx_rollup
open Plompiler
open Plonk_test.Helpers
open P
module TP = Types.P
module HashPV = Plompiler.Anemoi128
module SchnorrPV = Plompiler.Schnorr (HashPV)
module Schnorr = SchnorrPV.P

let sks : Schnorr.sk array =
  Array.init Constants.max_nb_accounts (fun _ ->
      Mec.Curve.Jubjub.AffineEdwards.Scalar.random ())

let pks = Array.map Schnorr.neuterize sks

module InvalidTx (L : LIB) = struct
  open Utils (L)

  open Helpers.V (L)

  open V (L)

  let tez i =
    TP.
      {
        id = Epoxy_tx.Constants.tez_id;
        amount = Bounded.make ~bound:Constants.Bound.max_amount (Z.of_int i);
      }

  let get_transfer (r, state) =
    let tx = match r.tx with Transfer tx -> tx | _ -> assert false in
    (tx, state)

  (* Insufficient balance:
     User 0 attempts to send 100 to user 1, but only has a balance of 99
  *)
  let tests_insufficient_balace =
    let state =
      P.make_state Z.[(pks.(0), of_int 99, [||]); (pks.(1), of_int 100, [||])]
    in
    let r1, state1 =
      P.generate_transaction
        ~valid:false
        ~unsafe:true
        ~src_pos:Z.zero
        ~dst_pos:Z.(of_int Constants.max_nb_tickets)
        ~amount:(tez 100)
        ~fee:Z.zero
        ~sks
        state
    in
    let r2, state2 =
      P.generate_transaction
        ~src_pos:Z.zero
        ~dst_pos:Z.(of_int Constants.max_nb_tickets)
        ~amount:(tez 80)
        ~fee:Z.(of_int 20)
        ~valid:false
        ~unsafe:true
        ~sks
        state
    in
    [
      test
        ~valid:true
        ~name:"insufficient balance: amount > bal"
        (circuit_op r1 state state1);
      test
        ~valid:true
        ~name:"insufficient balance: amount + fee > bal"
        (circuit_op r2 state state2);
    ]

  (* Dst doesn't exist *)
  let tests_invalid_dst =
    let state = P.make_state [(pks.(0), Z.of_int 100, [||])] in
    let r, state' =
      P.generate_transaction
        ~src_pos:Z.zero
        ~dst_pos:Z.(of_int Constants.max_nb_tickets)
        ~amount:(tez 10)
        ~fee:Z.zero
        ~valid:false
        ~unsafe:true
        ~sks
        state
    in
    [test ~valid:true ~name:"invalid dst" (circuit_op r state state')]

  (* Src doesn't exist *)

  let tests_invalid_src =
    let state = P.make_state [(pks.(0), Z.of_int 100, [||])] in
    let r, state' =
      P.generate_transaction
        ~src_pos:Z.(of_int Constants.max_nb_tickets)
        ~dst_pos:Z.zero
        ~amount:(tez 10)
        ~fee:Z.zero
        ~valid:false
        ~unsafe:true
        ~sks
        state
    in
    [test ~valid:true ~name:"invalid src" (circuit_op r state state')]

  (* Replay *)
  let tests_replay =
    let state =
      P.make_state Z.[(pks.(0), of_int 100, [||]); (pks.(1), of_int 100, [||])]
    in
    let _r, state1 =
      P.generate_transaction
        ~src_pos:Z.zero
        ~dst_pos:Z.(of_int Constants.max_nb_tickets)
        ~amount:(tez 10)
        ~fee:Z.zero
        ~cnt:Z.zero
        ~valid:true
        ~unsafe:true
        ~sks
        state
    in
    let r, state' =
      P.generate_transaction
        ~src_pos:Z.zero
        ~dst_pos:Z.(of_int Constants.max_nb_tickets)
        ~amount:(tez 10)
        ~fee:Z.zero
        ~cnt:Z.zero
        ~valid:false
        ~unsafe:true
        ~sks
        state1
    in
    [test ~valid:true ~name:"replay" (circuit_op r state1 state')]

  (*  New balance out of bounds *)
  let tests_out_of_bounds =
    let state =
      P.make_state
        Z.
          [
            (pks.(0), of_int 100, [||]);
            (pks.(1), Constants.Bound.(v max_balance) - of_int 50, [||]);
          ]
    in
    let r, state' =
      P.generate_transaction
        ~src_pos:Z.zero
        ~dst_pos:Z.(of_int Constants.max_nb_tickets)
        ~amount:(tez 60)
        ~fee:Z.zero
        ~valid:false
        ~unsafe:true
        ~sks
        state
    in
    [
      test
        ~valid:true
        ~name:"balance out of bounds 1"
        (circuit_op r state state');
    ]

  (* Invalid signature *)
  let tests_invalid_signature =
    let state = P.make_state Z.[(pks.(0), of_int 100, [||])] in
    let r, state' =
      P.generate_transaction
        ~src_pos:Z.zero
        ~dst_pos:Z.(of_int Constants.max_nb_tickets)
        ~amount:(tez 50)
        ~fee:Z.zero
        ~valid:false
        ~unsafe:true
        ~sks
        state
    in
    let tx, state' = get_transfer (r, state') in
    let bad_msg =
      let compressed_msg =
        P.compress
          TP.Bounded.
            [
              f tx.header.op_code;
              f tx.header.price.amount;
              f tx.payload.msg.dst;
              (* change order*)
              f tx.payload.msg.src;
              f tx.payload.msg.fee;
              f tx.payload.msg.amount.amount;
              f tx.payload.msg.cnt;
            ]
      in
      HashPV.P.direct
        ~input_length:4
        [|
          Epoxy_tx.Utils.scalar_of_bytes tx.header.l1_dst;
          S.of_z compressed_msg;
          tx.header.price.id;
          tx.payload.msg.amount.id;
        |]
    in
    let rand = Mec.Curve.Jubjub.AffineEdwards.Scalar.random () in
    let bad_msg_signature =
      SchnorrPV.P.sign ~compressed:true sks.(0) bad_msg rand
    in
    let tx_bad_msg =
      {tx with payload = {tx.payload with signature = bad_msg_signature}}
    in
    (* TODO: fix this *)
    let msg =
      let compressed_msg =
        P.compress
          TP.Bounded.
            [
              f tx.header.op_code;
              f tx.header.price.amount;
              f tx.payload.msg.src;
              f tx.payload.msg.dst;
              f tx.payload.msg.fee;
              f tx.payload.msg.amount.amount;
              f tx.payload.msg.cnt;
            ]
      in
      HashPV.P.direct
        ~input_length:4
        [|
          Epoxy_tx.Utils.scalar_of_bytes tx.header.l1_dst;
          S.of_z compressed_msg;
          tx.header.price.id;
          tx.payload.msg.amount.id;
        |]
    in
    let bad_sk_signature = SchnorrPV.P.sign ~compressed:true sks.(1) msg rand in
    let tx_bad_sk =
      {tx with payload = {tx.payload with signature = bad_sk_signature}}
    in
    let tx_not_on_curve =
      let bad_r =
        Mec.Curve.Jubjub.AffineEdwards.(
          unsafe_from_coordinates ~u:(Base.of_z Z.one) ~v:(Base.of_z Z.one))
      in
      {
        tx with
        payload =
          {
            tx.payload with
            signature = {tx.payload.signature with sig_r = bad_r};
          };
      }
    in

    [
      test
        ~valid:true
        ~name:"invalid signature: bad msg"
        (circuit_op {r with tx = Transfer tx_bad_msg} state state');
      test
        ~valid:true
        ~name:"invalid signature: bad sk"
        (circuit_op {r with tx = Transfer tx_bad_sk} state state');
      test
        ~valid:true
        ~name:"invalid signature: not on curve"
        (circuit_op {r with tx = Transfer tx_not_on_curve} state state');
    ]

  let tests =
    tests_insufficient_balace @ tests_invalid_dst @ tests_invalid_src
    @ tests_replay @ tests_out_of_bounds @ tests_invalid_signature
end

let tests =
  [
    Alcotest.test_case "InvalidTx" `Quick (to_test (module InvalidTx : Test));
    Alcotest.test_case
      "InvalidTx plonk"
      `Slow
      (to_test ~plonk:(module Plonk.Main_protocol) (module InvalidTx : Test));
  ]
