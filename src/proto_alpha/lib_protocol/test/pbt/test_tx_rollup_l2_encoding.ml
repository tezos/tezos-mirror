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
    Invocation:   dune exec \
                  src/proto_alpha/lib_protocol/test/pbt/test_tx_rollup_l2_encoding.exe
    Subject:      Tx rollup l2 encoding
*)

open Lib_test
open Lib_test.Qcheck2_helpers
open Protocol.Indexable
open Protocol.Tx_rollup_l2_batch

(* ------ generators and compact encodings ---------------------------------- *)

let seed_gen =
  let open QCheck2.Gen in
  return @@ Bytes.init 32 (fun _ -> generate1 char)

let bls_pk_gen =
  let open QCheck2.Gen in
  let+ seed = seed_gen in
  let secret_key = Bls12_381.Signature.generate_sk seed in
  Bls12_381.Signature.MinPk.derive_pk secret_key

let signer_gen : Signer_indexable.either QCheck2.Gen.t =
  let open QCheck2.Gen in
  let* choice = bool in
  if choice then (fun pk -> from_value pk) <$> bls_pk_gen
  else (fun x -> from_index_exn x) <$> ui32

let l2_address_gen =
  let open QCheck2.Gen in
  Protocol.Tx_rollup_l2_address.of_bls_pk <$> bls_pk_gen

let destination_gen =
  let open QCheck2.Gen in
  let* choice = bool in
  if choice then
    return
    @@ Layer1
         (Signature.Public_key_hash.of_b58check_exn
            "tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU")
  else
    let* choice = bool in
    if choice then (fun x -> Layer2 (from_index_exn x)) <$> ui32
    else (fun x -> Layer2 (from_value x)) <$> l2_address_gen

let ticket_hash_gen =
  let open QCheck2.Gen in
  from_index_exn <$> ui32

let qty_gen =
  let open QCheck2.Gen in
  Protocol.Tx_rollup_l2_qty.of_int64_exn
  <$> graft_corners ui64 [0L; 1L; 2L; Int64.max_int] ()

let v1_operation_content_gen =
  let open QCheck2.Gen in
  let+ destination = destination_gen
  and+ ticket_hash = ticket_hash_gen
  and+ qty = qty_gen in
  V1.{destination; ticket_hash; qty}

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
  let+ contents = small_list v1_transaction_gen
  and+ aggregated_signature = bytes_gen in
  V1.{aggregated_signature; contents}

let batch =
  let open QCheck2.Gen in
  (fun batch -> V1 batch) <$> v1_batch

let pp fmt _ = Format.fprintf fmt "{}"

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

let test_roundtrip ~count title arb equ pp encoding =
  let test rdt input =
    let output = Roundtrip.make encoding rdt input in
    let success = equ input output in
    if not success then
      QCheck2.Test.fail_reportf
        "%s %s roundtrip error: %a became %a"
        title
        (Roundtrip.target rdt)
        pp
        input
        pp
        output
  in
  QCheck2.Test.make
    ~count
    ~name:(Format.asprintf "roundtrip %s" title)
    arb
    (fun input ->
      test Roundtrip.binary input ;
      test Roundtrip.json input ;
      true)

let () =
  let qcheck_wrap = qcheck_wrap ~rand:(Random.State.make_self_init ()) in
  Alcotest.run
    "Compact_encoding"
    [
      ("quantity", qcheck_wrap [test_quantity ~count:100_000]);
      ( "roundtrip",
        qcheck_wrap
          [
            test_roundtrip
              ~count:1_000
              "batch"
              batch
              ( = )
              pp
              Protocol.Tx_rollup_l2_batch.encoding;
          ] );
    ]
