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

open Protocol

type t = {
  transaction :
    (Indexable.unknown, Indexable.unknown) Tx_rollup_l2_batch.V1.transaction;
  signatures : Tx_rollup_l2_batch.V1.signature list;
}

let encoding =
  let open Data_encoding in
  conv
    (fun {transaction; signatures} -> (transaction, signatures))
    (fun (transaction, signatures) -> {transaction; signatures})
  @@ obj2
       (req "transaction" Tx_rollup_l2_batch.V1.transaction_encoding)
       (req "signatures" (list Bls.encoding))

let batch l =
  let contents = List.map (fun {transaction; _} -> transaction) l in
  let aggregated_signature =
    List.concat_map (fun {signatures; _} -> signatures) l
    |> Environment.Bls_signature.aggregate_signature_opt
  in
  match aggregated_signature with
  | None -> error_with "Cannot aggregate signatures"
  | Some aggregated_signature ->
      ok Tx_rollup_l2_batch.(V1 V1.{contents; aggregated_signature})

module Hash =
  Blake2B.Make
    (Base58)
    (struct
      let name = "tx_rollup_l2_transaction_hash"

      let title = "An tx_rollup L2 transaction"

      let b58check_prefix = "\018\007\031\191" (* txL2(54) *)

      let size = None
    end)

let () = Base58.check_encoded_prefix Hash.b58check_encoding "txL2" 54

type hash = Hash.t

let hash t = Hash.hash_bytes [Data_encoding.Binary.to_bytes_exn encoding t]
