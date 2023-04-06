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
                  src/proto_alpha/lib_protocol/test/pbt/test_zk_rollup_encoding.exe
    Subject:      Zk rollup encodings
*)

open Protocol
open QCheck2
open Qcheck2_helpers

(* Generators *)

let gen_zkr_address =
  let open Gen in
  let+ bytes = bytes_fixed_gen Zk_rollup_repr.Address.size in
  Zk_rollup_repr.Address.of_bytes_exn bytes

let gen_scalar =
  let s = Bls12_381.Fr.random () in
  Gen.return s

let gen_l2_state =
  let open Gen in
  array gen_scalar

(* Number of operations in each private batch *)
let batch_size = 10

(* We use fixed values for Plonk types, because it's interface
   doesn't expose a quick and safe way to create them randomly. *)
module Operator = Dummy_zk_rollup.Operator (struct
  let batch_size = batch_size
end)

let nat64 = int64_range_gen 0L Int64.max_int

let gen_zkr_account =
  let open Gen in
  let open Zk_rollup_account_repr in
  let* state = gen_l2_state in
  let _prover_pp, public_parameters = Lazy.force Operator.lazy_pp in
  let circuits_info = SMap.of_seq (Plonk.SMap.to_seq Operator.circuits) in
  let* nb_ops = nat in
  let static =
    {
      public_parameters;
      state_length = Array.length state;
      circuits_info;
      nb_ops;
    }
  in
  let* paid_l2_operations_storage_space = nat in
  let+ used_l2_operations_storage_space =
    map Z.of_int @@ int_bound paid_l2_operations_storage_space
  in
  let dynamic =
    {
      state;
      paid_l2_operations_storage_space =
        Z.of_int paid_l2_operations_storage_space;
      used_l2_operations_storage_space;
    }
  in
  {static; dynamic}

let gen_ticket_hash =
  let open Gen in
  let+ bytes = bytes_fixed_gen Script_expr_hash.size in
  Ticket_hash_repr.of_bytes_exn bytes

let gen_pkh =
  let pkh, _, _ = Signature.generate_key ~algo:Ed25519 () in
  Gen.return pkh

let gen_z =
  let open Gen in
  sized @@ fun n -> map Z.of_bits (string_size (return n))

let gen_l2_op =
  let open Gen in
  let* op_code = nat in
  let* price =
    map2
      (fun id amount -> Zk_rollup_operation_repr.{id; amount})
      gen_ticket_hash
      gen_z
  in
  let* l1_dst = gen_pkh in
  let* rollup_id = gen_zkr_address in
  let+ payload = array gen_scalar in
  Zk_rollup_operation_repr.{op_code; price; l1_dst; rollup_id; payload}

let gen_pending_list =
  let open Gen in
  let open Zk_rollup_repr in
  let of_length next_index = function
    | 0 -> Empty {next_index}
    | length -> Pending {next_index; length}
  in
  map2 of_length nat64 uint16

(* Data-encoding roundtrip tests *)

let test_roundtrip_address =
  test_roundtrip
    ~count:1_000
    ~title:"Zk_rollup.t"
    ~gen:gen_zkr_address
    ~eq:( = )
    Zk_rollup_repr.Address.encoding

let test_roundtrip_state =
  test_roundtrip
    ~count:1_000
    ~title:"Zk_rollup_state_repr.t"
    ~gen:gen_l2_state
    ~eq:( = )
    Zk_rollup_state_repr.encoding

let eq_account acc0 acc1 =
  let open Zk_rollup_account_repr in
  let pp_to_bytes pp =
    Data_encoding.Binary.to_bytes_exn
      Environment.Plonk.public_parameters_encoding
      pp
  in
  acc0.dynamic = acc1.dynamic
  && acc0.static.state_length = acc1.static.state_length
  && acc0.static.circuits_info = acc1.static.circuits_info
  && acc0.static.nb_ops = acc1.static.nb_ops
  && pp_to_bytes acc0.static.public_parameters
     = pp_to_bytes acc1.static.public_parameters

let test_roundtrip_account =
  test_roundtrip
    ~count:1_000
    ~title:"Zk_rollup_account_repr.t"
    ~gen:gen_zkr_account
    ~eq:eq_account
    Zk_rollup_account_repr.encoding

let test_roundtrip_operation =
  test_roundtrip
    ~count:1_000
    ~title:"Zk_rollup_operation_repr.t"
    ~gen:gen_l2_op
    ~eq:( = )
    Zk_rollup_operation_repr.encoding

let test_roundtrip_pending_list =
  test_roundtrip
    ~count:1_000
    ~title:"Zk_rollup_repr.pending_list"
    ~gen:gen_pending_list
    ~eq:( = )
    Zk_rollup_repr.pending_list_encoding

let tests_roundtrip =
  [
    test_roundtrip_address;
    test_roundtrip_state;
    test_roundtrip_account;
    test_roundtrip_operation;
    test_roundtrip_pending_list;
  ]

(* Scalar conversion tests *)

let test_to_scalar ~count ~title ~gen to_scalar =
  QCheck2.Test.make
    ~count
    ~name:(Format.asprintf "to_scalar %s" title)
    gen
    (fun input ->
      try
        ignore @@ to_scalar input ;
        true
      with _ -> false)

let test_address_to_scalar =
  test_to_scalar
    ~count:1_000
    ~title:"Zk_rollup_repr.t"
    ~gen:gen_zkr_address
    Zk_rollup_repr.to_scalar

let test_operation_to_scalar =
  test_to_scalar
    ~count:1_000
    ~title:"Zk_rollup_operation.t"
    ~gen:gen_l2_op
    Zk_rollup_operation_repr.to_scalar_array

let tests_to_scalar = [test_address_to_scalar; test_operation_to_scalar]

let () =
  Alcotest.run
    ~__FILE__
    "ZK rollup encoding"
    [
      (Protocol.name ^ ": roundtrip", qcheck_wrap tests_roundtrip);
      (Protocol.name ^ ": to_scalar", qcheck_wrap tests_to_scalar);
    ]
