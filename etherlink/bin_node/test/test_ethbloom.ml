(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Bin_evm_node
    Invocation:   dune exec etherlink/bin_evm_node/test/main.exe -- --file test_ethbloom.ml
    Subject:      Tests for implementation of Ethereum bloom filters
*)

open Evm_node_lib_dev_encoding
open Evm_node_lib_dev.Ethbloom
open Ethereum_types

let test_empty_is_empty () =
  let empty = make () in
  Check.is_true (is_empty empty) ~error_msg:"Empty filter should be empty"

let test_accrue_not_empty () =
  let filter = make () in
  let input = "556E20666573746976616C2064652047414454" |> hex_of_string in
  accrue ~input filter ;
  Check.is_false
    (is_empty filter)
    ~error_msg:"Filter shouldn't be empty after accrue"

let test_contains_on_empty () =
  let input = "556E20666573746976616C2064652047414454" |> hex_of_string in
  let empty = make () in
  Check.is_false
    (contains_input ~input empty)
    ~error_msg:"Empty filter shouldn't contain any input" ;
  let filter = make () in
  accrue ~input filter ;
  Check.is_false
    (contains_bloom empty filter)
    ~error_msg:"Empty filter shouldn't contain a non-empty one"

let test_accrue_contains () =
  let input = "556E20666573746976616C2064652047414454" |> hex_of_string in
  let filter = make () in
  accrue ~input filter ;
  Check.is_true
    (contains_input ~input filter)
    ~error_msg:"Filter should contain added input" ;
  Check.is_true
    (contains_bloom filter filter)
    ~error_msg:"Filter should contain itself"

let test_multiple_values () =
  let input1 = "556E20666573746976616C2064652047414454" |> hex_of_string in
  let input2 = "48656C6C6F20776F726C64" |> hex_of_string in
  let filter1, filter2 = (make (), make ()) in
  accrue ~input:input1 filter1 ;
  accrue ~input:input2 filter2 ;
  Check.is_false
    (contains_input ~input:input1 filter2)
    ~error_msg:"Filter2 shouldn't contain input1" ;
  Check.is_false
    (contains_bloom filter2 filter1)
    ~error_msg:"Filter2 shouldn't contain filter1" ;
  Check.is_false
    (contains_input ~input:input2 filter1)
    ~error_msg:"Filter1 shouldn't contain input2" ;
  Check.is_false
    (contains_bloom filter1 filter2)
    ~error_msg:"Filter1 shouldn't contain filter2" ;
  (* Expand filter1 with filter2's elements *)
  accrue_bloom filter1 filter2 ;
  Check.is_true
    (contains_input ~input:input2 filter1)
    ~error_msg:"Filter1 should now contain input2" ;
  Check.is_true
    (contains_input ~input:input1 filter1)
    ~error_msg:"Filter1 should still contain input1" ;
  Check.is_true
    (contains_bloom filter1 filter2)
    ~error_msg:"Filter1 should now contain filter2"

(* Reference filters computed using the ethbloom Rust crate. *)
let test_externally_computed () =
  let ref_filter1 =
    Hex
      "00000000000000000000000000000000000000000000000000000000000000000000000080000000000000000000000200000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    |> hex_to_bytes
  in
  let ref_filter2 =
    Hex
      "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000000000000000000100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    |> hex_to_bytes
  in
  let ref_union =
    Hex
      "00000000000000000000000000000000000000000000000000000000000000000000000080000000000000000000000200000000000000000000000000000000000000000000000001000000000000000000100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    |> hex_to_bytes
  in
  let input1 = "556E20666573746976616C2064652047414454" |> hex_of_string in
  let input2 = "48656C6C6F20776F726C64" |> hex_of_string in
  let filter1, filter2 = (make (), make ()) in
  accrue ~input:input1 filter1 ;
  accrue ~input:input2 filter2 ;
  Check.(
    (Bytes.to_string filter1 = ref_filter1)
      string
      ~error_msg:"Expected filter to be %%R but got %%L.") ;
  Check.(
    (Bytes.to_string filter2 = ref_filter2)
      string
      ~error_msg:"Expected filter to be %%R but got %%L.") ;
  accrue_bloom filter1 filter2 ;
  Check.(
    (Bytes.to_string filter1 = ref_union)
      string
      ~error_msg:"Expected filter to be %%R but got %%L.")

(* Test from Rust crate,
   https://github.com/paritytech/parity-common/blob/master/ethbloom/src/lib.rs#L282 *)
let test_external_data () =
  let ref_filter =
    Hex
      "00000000000000000000000000000000000000001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000002020000000000000000000000000000000000000000000008000000001000000000000000000000000000000000000000000000000000001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    |> hex_to_bytes |> Bytes.of_string
  in
  let address = Hex "ef2d6d194084c2de36e0dabfce45d046b37d1106" in
  let topic =
    Hex "02c69be41d0b7e40352fc85be1cd65eb03d40ef8427a0ca4596b1ead9a00e9fc"
  in
  (* Empty doesn't contain inputs *)
  let my_bloom = make () in
  Check.is_false
    (contains_input ~input:address my_bloom)
    ~error_msg:"Empty filter shouldn't contain address" ;
  Check.is_false
    (contains_input ~input:topic my_bloom)
    ~error_msg:"Empty filter shouldn't contain topic" ;
  (* Accrue address *)
  accrue ~input:address my_bloom ;
  Check.is_true
    (contains_input ~input:address my_bloom)
    ~error_msg:"Filter should contain address" ;
  Check.is_false
    (contains_input ~input:topic my_bloom)
    ~error_msg:"Filter shouldn't contain topic" ;
  (* Accrue topic *)
  accrue ~input:topic my_bloom ;
  Check.is_true
    (contains_input ~input:address my_bloom)
    ~error_msg:"Filter should contain address" ;
  Check.is_true
    (contains_input ~input:topic my_bloom)
    ~error_msg:"Filter should contain topic" ;
  (* Compare to reference *)
  Check.(
    (Bytes.to_string my_bloom = Bytes.to_string ref_filter)
      string
      ~error_msg:"Expected filter to be %%R but got %%L.")

let tests =
  [
    ( "Internal",
      [
        ("test_empty_is_empty", `Quick, test_empty_is_empty);
        ("test_accrue_not_empty", `Quick, test_accrue_not_empty);
        ("test_contains_on_empty", `Quick, test_contains_on_empty);
        ("test_accrue_contains", `Quick, test_accrue_contains);
        ("test_multiple_values", `Quick, test_multiple_values);
      ] );
    ( "External",
      [
        ("test_externally_computed", `Quick, test_externally_computed);
        ("test_external_data", `Quick, test_external_data);
      ] );
  ]

let () = Alcotest.run ~__FILE__ "Test Ethbloom" tests
