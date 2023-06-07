(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs. <contact@nomadic-labs.com>               *)
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
    Component:    Destination_repr
    Invocation:   dune exec src/proto_017_PtNairob/lib_protocol/test/unit/main.exe \
                  -- --file test_destination_repr.ml
    Subject:      To test the encoding of [Destination_repr] and assert it is
                  compatible with [Contract_repr.encoding].
*)

open Protocol
open Tztest

let dummy_operation_hash =
  Operation_hash.of_bytes_exn
    (Bytes.of_string "test-operation-hash-of-length-32")

let dummy_origination_nonce = Origination_nonce.initial dummy_operation_hash

let contracts =
  let since = dummy_origination_nonce in
  let rec incr_n_times nonce = function
    | 0 -> nonce
    | n -> incr_n_times (Origination_nonce.incr nonce) (n - 1)
  in
  let until = incr_n_times since 5 in
  Contract_repr.originated_contracts ~since ~until
  |> List.map (fun c -> Contract_repr.Originated c)

let dest x = Destination_repr.Contract x

let construct = Data_encoding.Json.construct

let destruct = Data_encoding.Json.destruct

let to_bytes_exn = Data_encoding.Binary.to_bytes_exn

let of_bytes_exn = Data_encoding.Binary.of_bytes_exn

let ( !! ) = function Ok x -> x | Error _ -> raise (Invalid_argument "( !! )")

(* The following addresses have been extracted from TzKT. *)

let null_address = "tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU"

let liquidity_baking_dex = "KT1TxqZ8QtKvLu3V3JH7Gx58n7Co8pgtpQU5"

(* The following address has been extracted from
   [tezt/_regressions/tx_rollup_simple_use_case.out]. *)

let tx_rollup_address = "txr1YNMEtkj5Vkqsbdmt7xaxBTMRZjzS96UAi"

(* The following address has been extracted like this:
     - [dune exec tezt/tests/main.exe -- -verbose --file sc_rollup.ml
        sc_rollup list]
*)
let sc_rollup_address = "sr1BAwv191dVYeZg44ZxVy8dFwfRQKW6bSqc"

(* TODO: https://gitlab.com/tezos/tezos/-/issues/3731
   Explain how this address was computed *)
let zk_rollup_address = "epx18RJJqrYuJQqhB636BWvukU3XBNQGbtm8C"

let assert_compat contract destination =
  match destination with
  | Destination_repr.Contract contract'
    when Contract_repr.equal contract contract' ->
      ()
  | _ -> raise (Invalid_argument "assert_compat")

(** [test_decoding_json_compat str] decodes [str] as both a [Destination_repr.t]
    and [Contract_repr.t], and checks the two are equal. *)
let test_decoding_json_compat str () =
  let json =
    !!(Data_encoding.Json.from_string @@ Format.sprintf {|"%s"|} str)
  in
  let contract = destruct Contract_repr.encoding json in
  let destination = destruct Destination_repr.encoding json in

  assert_compat contract destination ;

  return_unit

(** [test_encode_contract_decode_destination str] interprets [str] as
    a [Contract_repr.t], encodes it in a bytes array, then decodes it
    as a [Destination_repr.t]. The resulting destination should be
    equal to the initial contract. *)
let test_encode_contract_decode_destination str () =
  let contract = !!(Contract_repr.of_b58check str) in
  let bytes = to_bytes_exn Contract_repr.encoding contract in
  let destination = of_bytes_exn Destination_repr.encoding bytes in

  assert_compat contract destination ;

  return_unit

(** [test_encode_destination_decode_contract str] interprets [str] as
    a [Destination_repr.t], encodes it in a bytes array, then decodes
    it as a [Contract_repr.t]. The resulting contract should be equal
    to the initial destination. *)
let test_encode_destination_decode_contract str () =
  let destination = !!(Destination_repr.of_b58check str) in
  let bytes = to_bytes_exn Destination_repr.encoding destination in
  let contract = of_bytes_exn Contract_repr.encoding bytes in

  assert_compat contract destination ;

  return_unit

let encoding_compat ~encode_contract ~decode_contract ~encode_destination
    ~decode_destination contract =
  let destination = dest contract in

  let encoded_contract = encode_contract contract in
  let encoded_destination = encode_destination destination in

  let destination_of_contract = decode_destination encoded_contract in
  let contract_of_destination = decode_contract encoded_destination in

  assert_compat contract_of_destination destination ;
  assert_compat contract destination_of_contract ;

  return_unit

(** [encoding_json_compat contract] creates a {!Destination_repr.t} using
    a dummy contract and ensures that their JSON encodings are compatible with
    each other.
*)
let encoding_json_compat contract =
  encoding_compat
    ~encode_contract:(construct Contract_repr.encoding)
    ~decode_contract:(destruct Contract_repr.encoding)
    ~encode_destination:(construct Destination_repr.encoding)
    ~decode_destination:(destruct Destination_repr.encoding)
    contract

(** [encoding_json_compat contract] creates a {!Destination_repr.t} using
    a dummy contract and ensures that their binary encodings are compatible with
    each other.
*)
let encoding_binary_compat contract =
  encoding_compat
    ~encode_contract:(to_bytes_exn Contract_repr.encoding)
    ~decode_contract:(of_bytes_exn Contract_repr.encoding)
    ~encode_destination:(to_bytes_exn Destination_repr.encoding)
    ~decode_destination:(of_bytes_exn Destination_repr.encoding)
    contract

let test_contracts f () =
  List.iter (fun contract -> ignore (f contract)) contracts ;

  return_unit

let test_encoding_binary_compat = test_contracts encoding_binary_compat

let test_encoding_json_compat = test_contracts encoding_json_compat

let test_compare_destination () =
  let tz1 = !!(Destination_repr.of_b58check null_address) in
  let kt1 = !!(Destination_repr.of_b58check liquidity_baking_dex) in
  let txr1 = !!(Destination_repr.of_b58check tx_rollup_address) in
  let scr1 = !!(Destination_repr.of_b58check sc_rollup_address) in
  let epx1 = !!(Destination_repr.of_b58check zk_rollup_address) in

  assert (Destination_repr.(tz1 < kt1)) ;
  assert (Destination_repr.(kt1 < txr1)) ;
  assert (Destination_repr.(tz1 < txr1)) ;
  assert (Destination_repr.(txr1 < scr1)) ;
  assert (Destination_repr.(scr1 < epx1)) ;

  return_unit

let tests =
  [
    tztest "Json decoding compat implicit contract (null address)" `Quick
    @@ test_decoding_json_compat null_address;
    tztest "Json decoding compat smart contract (liquidity baking dex)" `Quick
    @@ test_decoding_json_compat liquidity_baking_dex;
    tztest "Binary Contract_repr to Destination_repr (null address)" `Quick
    @@ test_encode_contract_decode_destination null_address;
    tztest
      "Binary Contract_repr to Destination_repr (liquidity baking dex)"
      `Quick
    @@ test_encode_contract_decode_destination liquidity_baking_dex;
    tztest "Binary Destination_repr to Contract_repr (null address)" `Quick
    @@ test_encode_destination_decode_contract null_address;
    tztest
      "Binary Destination_repr to Contract_repr (liquidity baking dex)"
      `Quick
    @@ test_encode_destination_decode_contract liquidity_baking_dex;
    tztest
      "Json encoding compatibility Contract_repr to Destination_repr with \
       dummy contracts"
      `Quick
    @@ test_encoding_json_compat;
    tztest
      "Binary encoding compatibility Contract_repr to Destination_repr with \
       dummy contracts"
      `Quick
    @@ test_encoding_json_compat;
    tztest "Comparison of destinations" `Quick test_compare_destination;
  ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("Destination_repr.ml", tests)]
  |> Lwt_main.run
