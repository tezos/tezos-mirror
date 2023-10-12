(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Marigold <contact@marigold.dev>                        *)
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
    Component:    Operation_repr
    Invocation:   dune exec src/proto_alpha/lib_protocol/test/unit/main.exe \
                  -- --file test_operation_repr.ml
    Dependencies: --
    Subject:      To test the modules (including the top-level)
                  in operation_repr.ml as individual units, particularly
                  failure cases. Superficial goal: increase coverage percentage.
*)
open Protocol

open Tztest

module Test_operation_repr = struct
  open Operation_repr

  let test_of_list_single_case () =
    let open Lwt_result_wrap_syntax in
    let op =
      Manager_operation
        {
          fee = Obj.magic 0;
          operation = Obj.magic 0;
          gas_limit = Obj.magic 0;
          storage_limit = Obj.magic 0;
          counter = Obj.magic 0;
          source = Obj.magic 0;
        }
    in
    let*?@ contents_list = of_list [Contents op] in
    match contents_list with
    | Contents_list (Single op') when Obj.repr op == Obj.repr op' -> return_unit
    | _ -> failwith "Unexpected value"

  let test_of_list_multiple_case () =
    let open Lwt_result_wrap_syntax in
    let op1 =
      Manager_operation
        {
          fee = Obj.magic 0;
          operation = Obj.magic 0;
          gas_limit = Obj.magic 0;
          storage_limit = Obj.magic 0;
          counter = Obj.magic 0;
          source = Obj.magic 0;
        }
    in
    let op2 =
      Manager_operation
        {
          fee = Obj.magic 1;
          operation = Obj.magic 0;
          gas_limit = Obj.magic 0;
          storage_limit = Obj.magic 0;
          counter = Obj.magic 0;
          source = Obj.magic 0;
        }
    in
    let*?@ contents_list = of_list [Contents op1; Contents op2] in
    match contents_list with
    | Contents_list (Cons (op1', Single op2'))
      when Obj.repr op1 == Obj.repr op1' && Obj.repr op2 == Obj.repr op2' ->
        return_unit
    | _ -> failwith "Unexpected value"

  let test_of_list_empty_case () =
    match of_list [] with
    | Ok _ -> failwith "of_list of an empty list was expected to fail"
    | Error _ -> return_unit

  let zero_bls =
    match Signature.(split_signature (Bls Signature.Bls.zero)) with
    | {prefix = None; _} -> assert false
    | {prefix = Some prefix; suffix} ->
        let prefix =
          Data_encoding.Binary.to_bytes_exn Signature.prefix_encoding prefix
        in
        (Bytes.cat (Bytes.of_string "\255") prefix, suffix)

  let test_split_signatures error assemble =
    let op_bytes =
      Data_encoding.Binary.to_bytes_exn
        Operation_repr.contents_encoding_with_legacy_attestation_name
        (Contents (Failing_noop ""))
    in
    let prefix, suffix = zero_bls in
    let protocol_data_bytes =
      Bytes.(concat empty) (assemble op_bytes prefix suffix)
    in
    match
      Data_encoding.Binary.of_bytes
        Operation_repr.protocol_data_encoding_with_legacy_attestation_name
        protocol_data_bytes
    with
    | Ok _ -> failwith "Should have failed with %s" error
    | Error (User_invariant_guard e) when e = error -> return_unit
    | Error e ->
        failwith
          "Unexpected error: %a instead of %s"
          Data_encoding.Binary.pp_read_error
          e
          error

  let test_only_signature_prefix () =
    test_split_signatures "Operation lists should not be empty."
    @@ fun _op_bytes prefix suffix -> [prefix; suffix]

  let test_decoding_empty_list () =
    test_split_signatures "Operation lists should not be empty."
    @@ fun _op_bytes _prefix suffix -> [suffix]

  let test_multiple_signature_prefix () =
    test_split_signatures "Signature prefix must appear last"
    @@ fun op_bytes prefix suffix -> [op_bytes; prefix; prefix; suffix]

  let test_signature_prefix_not_final () =
    test_split_signatures "Signature prefix must appear last"
    @@ fun op_bytes prefix suffix -> [prefix; op_bytes; suffix]

  let test_multiple_non_manager () =
    test_split_signatures
      "Operation list of length > 1 should only contain manager operations."
    @@ fun op_bytes prefix suffix -> [op_bytes; op_bytes; prefix; suffix]
end

let tests =
  [
    tztest
      "of_list: single element input list"
      `Quick
      Test_operation_repr.test_of_list_single_case;
    tztest
      "of_list: multiple element input list"
      `Quick
      Test_operation_repr.test_of_list_multiple_case;
    tztest
      "of_list: empty input list"
      `Quick
      Test_operation_repr.test_of_list_empty_case;
    tztest
      "protocol_data_encoding: only signature prefix"
      `Quick
      Test_operation_repr.test_only_signature_prefix;
    tztest
      "protocol_data_encoding: empty list"
      `Quick
      Test_operation_repr.test_decoding_empty_list;
    tztest
      "protocol_data_encoding: multiple signature prefix"
      `Quick
      Test_operation_repr.test_multiple_signature_prefix;
    tztest
      "protocol_data_encoding: signature prefix not final"
      `Quick
      Test_operation_repr.test_signature_prefix_not_final;
    tztest
      "protocol_data_encoding: multiple non manager"
      `Quick
      Test_operation_repr.test_multiple_non_manager;
  ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("Operation_repr.ml", tests)]
  |> Lwt_main.run
