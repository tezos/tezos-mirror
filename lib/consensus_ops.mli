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

val operation_kind_encoding : operation_kind Data_encoding.encoding

val pp_operation_kind : Format.formatter -> operation_kind -> unit

type operation = {
  hash : Operation_hash.t;
  kind : operation_kind;
  round : Int32.t option;
}

val operation_encoding : operation Data_encoding.encoding

type received_operation = {
  op : operation;
  reception_time : Time.System.t;
  errors : error list option;
}

type delegate_ops =
  (Tezos_crypto.Signature.Public_key_hash.t * received_operation list) list

val delegate_ops_encoding : delegate_ops Data_encoding.t

type block_op = {
  op : operation;
  delegate : Tezos_crypto.Signature.public_key_hash;
  power : int;
}

val block_op_encoding : block_op Data_encoding.encoding

type right = {
  address : Tezos_crypto.Signature.Public_key_hash.t;
  first_slot : int;
  power : int;
}

type rights = right list

val rights_encoding : rights Data_encoding.t
