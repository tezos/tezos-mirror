(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** Testing
    -------
    Component:    Protocol Library
    Invocation:   dune exec src/proto_016_PtMumbai/lib_protocol/test/pbt/main.exe \
                  -- --file test_tx_rollup_l2_encoding.ml
    Subject:      Tx rollup l2 encoding
*)

open Qcheck2_helpers
open Protocol.Indexable
open Protocol.Tx_rollup_l2_batch
open Protocol.Tx_rollup_l2_apply

(* ------ generators and compact encodings ---------------------------------- *)

let seed_gen = bytes_fixed_gen 32

let l2_address, bls_pk =
  (* Generating byte sequences in Qcheck2 is slow. We hard code one
     32byte IKMs: *)
  let ikm =
    `Hex "8fee216367c463821f82c942a1cee3a01469b1da782736ca269a2accea6e0cc4"
    |> Hex.to_bytes_exn
  in
  let pkh, public_key, _secret_key =
    Tezos_crypto.Signature.Bls.generate_key ~seed:ikm ()
  in
  (pkh, public_key)

let signer_gen : Signer_indexable.either QCheck2.Gen.t =
  let open QCheck2.Gen in
  frequency
    [
      (1, return @@ from_value (Bls_pk bls_pk));
      (5, return @@ from_value (L2_addr l2_address));
      (4, from_index_exn <$> ui32);
    ]

let signer_index_gen : Signer_indexable.index QCheck2.Gen.t =
  let open QCheck2.Gen in
  (fun x -> Protocol.Indexable.index_exn x) <$> ui32

let idx_l2_address_idx_gen =
  let open QCheck2.Gen in
  from_index_exn <$> ui32

let idx_l2_address_value = from_value l2_address

let idx_l2_address_gen =
  let open QCheck2.Gen in
  oneof [idx_l2_address_idx_gen; return idx_l2_address_value]

let public_key_hash =
  Tezos_crypto.Signature.Public_key_hash.of_b58check_exn
    "tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU"

let public_key_hash_gen =
  let open QCheck2.Gen in
  let+ seed = seed_gen in
  let pkh, _, _ = Tx_rollup_l2_helpers.gen_l1_address ~seed () in
  pkh

let ticket_hash : Protocol.Alpha_context.Ticket_hash.t =
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/2592
     we could introduce a bit more randomness here *)
  let ticketer_b58 = "tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU" in
  let ticketer_pkh =
    Tezos_crypto.Signature.Public_key_hash.of_b58check_exn ticketer_b58
  in
  let ticketer = Protocol.Alpha_context.Contract.Implicit ticketer_pkh in
  Tx_rollup_l2_helpers.make_unit_ticket_key ticketer l2_address

let idx_ticket_hash_idx_gen :
    Protocol.Alpha_context.Ticket_hash.t either QCheck2.Gen.t =
  let open QCheck2.Gen in
  from_index_exn <$> ui32

let idx_ticket_hash_value : Protocol.Alpha_context.Ticket_hash.t either =
  from_value ticket_hash

let idx_ticket_hash_gen :
    Protocol.Alpha_context.Ticket_hash.t either QCheck2.Gen.t =
  let open QCheck2.Gen in
  oneof [idx_ticket_hash_idx_gen; return idx_ticket_hash_value]

let qty_gen =
  let open QCheck2.Gen in
  Protocol.Tx_rollup_l2_qty.of_int64_exn
  <$> graft_corners ui64 [0L; 1L; 2L; Int64.max_int] ()

let v1_withdraw_gen =
  let open QCheck2.Gen in
  let+ destination = public_key_hash_gen and+ qty = qty_gen in
  V1.Withdraw {destination; ticket_hash; qty}

let v1_transfer_gen =
  let open QCheck2.Gen in
  let+ destination = idx_l2_address_gen
  and+ ticket_hash = idx_ticket_hash_gen
  and+ qty = qty_gen in
  V1.Transfer {destination; ticket_hash; qty}

let v1_operation_content_gen =
  QCheck2.Gen.oneof [v1_withdraw_gen; v1_transfer_gen]

let v1_operation_gen =
  let open QCheck2.Gen in
  let+ signer = signer_gen
  and+ counter = Int64.of_int <$> int
  and+ contents = small_list v1_operation_content_gen in
  V1.{signer; counter; contents}

let v1_transaction_gen =
  let open QCheck2.Gen in
  small_list v1_operation_gen

let v1_batch =
  let open QCheck2.Gen in
  let+ contents = small_list v1_transaction_gen in
  (* This it not ideal as we do not use the QCheck2 seed. We need
     valid bytes since the signature encoding is "safe" and accept
     only valid signatures. However, it should not impact the
     tests here as the bytes length stays the same. *)
  let bytes = Bls12_381.G2.(to_compressed_bytes (random ())) in
  let aggregated_signature =
    Bls12_381_signature.MinPk.unsafe_signature_of_bytes bytes
  in
  V1.{aggregated_signature; contents}

let batch =
  let open QCheck2.Gen in
  (fun batch -> V1 batch) <$> v1_batch

let indexes_gen =
  let open QCheck2.Gen in
  let ticket_hash_gen =
    match Tx_rollup_l2_helpers.gen_n_ticket_hash 1 with
    | [ticket] -> pure ticket
    | _ -> assert false
  in
  let* address_indexes =
    small_list
      (pair (return l2_address) (map Protocol.Indexable.index_exn ui32))
  in
  let+ ticket_indexes =
    small_list (pair ticket_hash_gen (map Protocol.Indexable.index_exn ui32))
  in
  {address_indexes; ticket_indexes}

let deposit_result_gen =
  let open QCheck2.Gen in
  let open Message_result in
  let success =
    let+ indexes = indexes_gen in
    Deposit_success indexes
  in
  (* We do no test here the encodings for every errors *)
  let failure =
    let error = Protocol.Tx_rollup_l2_apply.Incorrect_aggregated_signature in
    pure (Deposit_failure error)
  in
  let+ result = oneof [success; failure] in
  Deposit_result result

(** This is a particular transaction generator, the signers are provided
    with indexes only. *)
let v1_transaction_index_signer_gen :
    (index_only, unknown) V1.transaction QCheck2.Gen.t =
  let open QCheck2.Gen in
  let operation_signer_index_gen =
    let+ signer = signer_index_gen
    and+ counter = Int64.of_int <$> int
    and+ contents = small_list v1_operation_content_gen in
    V1.{signer; counter; contents}
  in
  small_list operation_signer_index_gen

let transaction_result_gen =
  let open QCheck2.Gen in
  let open Message_result in
  let success = pure Transaction_success in
  let failure =
    let reason = Protocol.Tx_rollup_l2_apply.Incorrect_aggregated_signature in
    let+ index = small_nat in
    Transaction_failure {index; reason}
  in
  oneof [success; failure]

let batch_v1_result_gen : Message_result.Batch_V1.t QCheck2.Gen.t =
  let open QCheck2.Gen in
  let* results =
    small_list (pair v1_transaction_index_signer_gen transaction_result_gen)
  in
  let+ indexes = indexes_gen in
  Message_result.Batch_V1.Batch_result {results; indexes}

let message_result : Message_result.message_result QCheck2.Gen.t =
  let open QCheck2.Gen in
  let open Message_result in
  let batch_v1_result_gen =
    let+ result = batch_v1_result_gen in
    Batch_V1_result result
  in
  frequency [(2, deposit_result_gen); (8, batch_v1_result_gen)]

let withdrawal : Protocol.Alpha_context.Tx_rollup_withdraw.t QCheck2.Gen.t =
  let open QCheck2.Gen in
  let open Protocol.Alpha_context.Tx_rollup_withdraw in
  let claimer = public_key_hash in
  let* amount = qty_gen in
  return {claimer; ticket_hash; amount}

let message_result_withdrawal : Message_result.t QCheck2.Gen.t =
  let open QCheck2.Gen in
  let+ mres = message_result and+ withdrawals = list withdrawal in
  (mres, withdrawals)

(* ------ test template ----------------------------------------------------- *)

let test_quantity ~count =
  let open Protocol in
  let open QCheck2.Gen in
  let op_gen = oneofl [`Sub; `Add] in
  let test_gen = triple op_gen qty_gen qty_gen in
  let print (op, q1, q2) =
    Format.asprintf
      "%a %s %a"
      Tx_rollup_l2_qty.pp
      q1
      (match op with `Add -> "+" | `Sub -> "-")
      Tx_rollup_l2_qty.pp
      q2
  in
  let test (op, q1, q2) =
    let f_op =
      match op with
      | `Sub -> Tx_rollup_l2_qty.sub
      | `Add -> Tx_rollup_l2_qty.add
    in
    match f_op q1 q2 with
    | Some q -> Tx_rollup_l2_qty.(q >= zero)
    | None -> (
        match op with
        | `Sub -> Tx_rollup_l2_qty.(q2 > q1)
        | `Add ->
            Int64.add
              (Tx_rollup_l2_qty.to_int64 q1)
              (Tx_rollup_l2_qty.to_int64 q2)
            < 0L)
  in
  QCheck2.Test.make ~count ~print ~name:"quantity operation" test_gen test

let () =
  let qcheck_wrap = qcheck_wrap ~rand:(Random.State.make_self_init ()) in
  Alcotest.run
    ~__FILE__
    (Protocol.name ^ ": Compact_encoding")
    [
      (": quantity", qcheck_wrap [test_quantity ~count:100_000]);
      ( ": roundtrip",
        qcheck_wrap
          [
            test_roundtrip
              ~count:100
              ~title:"batch"
              ~gen:batch
              ~eq:( = )
              Protocol.Tx_rollup_l2_batch.encoding;
            test_roundtrip
              ~count:100
              ~title:"message_result"
              ~gen:message_result_withdrawal
              ~eq:( = )
              Protocol.Tx_rollup_l2_apply.Message_result.encoding;
          ] );
    ]
