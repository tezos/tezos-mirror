(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2022 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

type right = {
  address : Tezos_crypto.Signature.Public_key_hash.t;
  first_slot : int;
  power : int;
}

let right_encoding =
  let open Data_encoding in
  conv
    (fun {address; first_slot; power} -> (address, first_slot, power))
    (fun (address, first_slot, power) -> {address; first_slot; power})
    (obj3
       (req "address" Tezos_crypto.Signature.Public_key_hash.encoding)
       (req "first_slot" int31)
       (req "power" int16))

type rights = right list

let rights_encoding = Data_encoding.list right_encoding

type operation_kind = Attestation | Preattestation

let operation_kind_encoding =
  let open Data_encoding in
  string_enum [("Endorsement", Attestation); ("Preendorsement", Preattestation)]

type operation = {
  hash : Operation_hash.t;
  kind : operation_kind;
  round : Int32.t option;
}

let operation_encoding =
  let open Data_encoding in
  conv
    (fun {hash; kind; round} -> (hash, kind, round))
    (fun (hash, kind, round) -> {hash; kind; round})
    (obj3
       (req "hash" Operation_hash.encoding)
       (req "kind" operation_kind_encoding)
       (opt "round" int32))

type received_operation = {
  op : operation;
  reception_time : Time.System.t;
  errors : error list option;
}

let received_operation_encoding =
  let open Data_encoding in
  conv
    (fun {op; reception_time; errors} -> (op, (reception_time, errors)))
    (fun (op, (reception_time, errors)) -> {op; reception_time; errors})
    (merge_objs
       operation_encoding
       (obj2
          (req "reception_time" Time.System.encoding)
          (dft "errors" Tezos_rpc.Error.opt_encoding None)))

type delegate_ops = (right * received_operation list) list

let delegate_ops_encoding =
  Data_encoding.(list (tup2 right_encoding (list received_operation_encoding)))

type block_op = {
  op : operation;
  delegate : Tezos_crypto.Signature.public_key_hash;
  power : int;
}

let block_op_encoding =
  let open Data_encoding in
  conv
    (fun {op; delegate; power} -> (op, delegate, power))
    (fun (op, delegate, power) -> {op; delegate; power})
    (obj3
       (req "operation" operation_encoding)
       (req "delegate" Tezos_crypto.Signature.Public_key_hash.encoding)
       (req "endorsing_power" int16))
