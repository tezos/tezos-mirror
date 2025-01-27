(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2022 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

type right = {
  address : Signature.Public_key_hash.t;
  first_slot : int;
  power : int;
}

type rights = right list

val rights_encoding : rights Data_encoding.t

type operation_kind = Attestation | Preattestation

val operation_kind_encoding : operation_kind Data_encoding.encoding

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

type delegate_ops = (right * received_operation list) list

val delegate_ops_encoding : delegate_ops Data_encoding.t

type block_op = {
  op : operation;
  delegate : Tezos_crypto.Signature.public_key_hash;
  power : int;
}

val block_op_encoding : block_op Data_encoding.encoding
