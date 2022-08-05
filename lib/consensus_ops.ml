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

type operation_kind = Endorsement | Preendorsement

let operation_kind_encoding =
  let open Data_encoding in
  string_enum [("Endorsement", Endorsement); ("Preendorsement", Preendorsement)]

let pp_operation_kind ppf kind =
  match kind with
  | Endorsement -> Format.fprintf ppf "Endorsement"
  | Preendorsement -> Format.fprintf ppf "Preendorsement"

type received_operation = {
  hash : Operation_hash.t;
  kind : operation_kind;
  round : Int32.t option;
  reception_time : Time.System.t;
  errors : error list option;
}

let received_operation_encoding =
  let open Data_encoding in
  conv
    (fun {hash; kind; round; reception_time; errors} ->
      (hash, kind, round, reception_time, errors))
    (fun (hash, kind, round, reception_time, errors) ->
      {hash; kind; round; reception_time; errors})
    (obj5
       (req "hash" Operation_hash.encoding)
       (req "kind" operation_kind_encoding)
       (opt "round" int32)
       (req "reception_time" Time.System.encoding)
       (dft "errors" RPC_error.opt_encoding None))

type delegate_ops = (Signature.Public_key_hash.t * received_operation list) list

let delegate_ops_encoding =
  Data_encoding.(
    list
      (tup2
         Signature.Public_key_hash.encoding
         (list received_operation_encoding)))

type block_op = {hash : Operation_hash.t; delegate : Signature.public_key_hash}

type block_info = {
  endorsements : block_op list;
  endorsements_round : Int32.t option;
  preendorsements : block_op list option;
  preendorsements_round : Int32.t option;
}

type right = {
  address : Signature.Public_key_hash.t;
  first_slot : int;
  power : int;
}

let right_encoding =
  let open Data_encoding in
  conv
    (fun {address; first_slot; power} -> (address, first_slot, power))
    (fun (address, first_slot, power) -> {address; first_slot; power})
    (obj3
       (req "address" Signature.Public_key_hash.encoding)
       (req "first_slot" int31)
       (req "power" int16))

type rights = right list

let rights_encoding = Data_encoding.list right_encoding
