(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
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
    Component:  Protocol (Rollup Management Protocol)
    Invocation: dune exec src/proto_alpha/lib_protocol/test/unit/main.exe \
                -- test "^\[Unit\] sc rollup management protocol$"
    Subject:    Sanity checks for the Rollup Management Protocol module.
*)

open Protocol
open Alpha_context

let wrap m = m >|= Environment.wrap_tzresult

let check_encode_decode_inbox_message message =
  let open Lwt_result_syntax in
  let open Sc_rollup_management_protocol in
  let*? bytes = Environment.wrap_tzresult @@ bytes_of_inbox_message message in
  let*? message' =
    Environment.wrap_tzresult @@ Internal_for_tests.inbox_message_of_bytes bytes
  in
  let*? bytes' = Environment.wrap_tzresult @@ bytes_of_inbox_message message' in
  Assert.equal_string
    ~loc:__LOC__
    (Bytes.to_string bytes)
    (Bytes.to_string bytes')

let check_encode_decode_outbox_message ctxt message =
  let open Lwt_result_syntax in
  let open Sc_rollup_management_protocol in
  let*? bytes =
    Environment.wrap_tzresult
    @@ Internal_for_tests.bytes_of_outbox_message message
  in
  let* (message', _ctxt) = wrap @@ outbox_message_of_bytes ctxt bytes in
  let*? bytes' =
    Environment.wrap_tzresult
    @@ Internal_for_tests.bytes_of_outbox_message message'
  in
  Assert.equal_string
    ~loc:__LOC__
    (Bytes.to_string bytes)
    (Bytes.to_string bytes')

let string_ticket ticketer contents amount =
  let open WithExceptions in
  let amount = Script_int.abs @@ Script_int.of_int amount in
  let ticketer = Result.get_ok ~loc:__LOC__ (Contract.of_b58check ticketer) in
  let contents =
    Result.get_ok ~loc:__LOC__ (Script_string.of_string contents)
  in
  Script_typed_ir.{ticketer; contents; amount}

let init_ctxt () =
  let open Lwt_result_syntax in
  let* (block, _baker, _contract, _src2) = Contract_helpers.init () in
  let+ incr = Incremental.begin_construction block in
  Incremental.alpha_ctxt incr

let test_encode_decode_inbox_message () =
  let open WithExceptions in
  let open Lwt_result_syntax in
  let* ctxt = init_ctxt () in
  let*? sender =
    Environment.wrap_tzresult
      (Contract.of_b58check "KT1BuEZtb68c1Q4yjtckcNjGELqWt56Xyesc")
  in
  let source =
    Result.get_ok
      ~loc:__LOC__
      (Signature.Public_key_hash.of_b58check
         "tz1RjtZUVeLhADFHDL8UwDZA6vjWWhojpu5w")
  in
  let*? (Script_typed_ir.Ty_ex_c pair_nat_ticket_string_ty) =
    Environment.wrap_tzresult
      (let open Result_syntax in
      let open Script_typed_ir in
      let* ticket_t = ticket_t (-1) string_t in
      pair_t (-1) nat_t ticket_t)
  in
  let payload =
    ( Script_int.(abs @@ of_int 42),
      string_ticket "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "red" 1 )
  in
  let* (deposit, _ctxt) =
    wrap
    @@ Sc_rollup_management_protocol.make_inbox_message
         ctxt
         pair_nat_ticket_string_ty
         ~payload
         ~sender
         ~source
  in
  check_encode_decode_inbox_message deposit

let test_encode_decode_outbox_message () =
  let open Lwt_result_syntax in
  let* ctxt = init_ctxt () in
  let*? (Script_typed_ir.Ty_ex_c pair_nat_ticket_string_ty) =
    Environment.wrap_tzresult
      (let open Result_syntax in
      let open Script_typed_ir in
      let* ticket_t = ticket_t (-1) string_t in
      pair_t (-1) nat_t ticket_t)
  in
  let parameters =
    ( Script_int.(abs @@ of_int 42),
      string_ticket "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "red" 1 )
  in
  let* (transaction1, ctxt) =
    let*? destination_contract =
      Environment.wrap_tzresult
        (Contract.of_b58check "KT1BuEZtb68c1Q4yjtckcNjGELqWt56Xyesc")
    in
    let destination = Destination.Contract destination_contract in
    wrap
    @@ Sc_rollup_management_protocol.Internal_for_tests.make_transaction
         ctxt
         pair_nat_ticket_string_ty
         ~parameters
         ~destination
         ~entrypoint:Entrypoint.default
  in
  let* (transaction2, ctxt) =
    let*? destination_contract =
      Environment.wrap_tzresult
        (Contract.of_b58check "KT1BuEZtb68c1Q4yjtckcNjGELqWt56Xyesc")
    in
    let destination = Destination.Contract destination_contract in
    wrap
    @@ Sc_rollup_management_protocol.Internal_for_tests.make_transaction
         ctxt
         Script_typed_ir.nat_t
         ~parameters:Script_int.(abs @@ of_int 10)
         ~destination
         ~entrypoint:Entrypoint.default
  in
  let outbox_message =
    Sc_rollup_management_protocol.Internal_for_tests.make_atomic_batch
      [transaction1; transaction2]
  in
  check_encode_decode_outbox_message ctxt outbox_message

let tests =
  [
    Tztest.tztest
      "Encode/decode inbox message"
      `Quick
      test_encode_decode_inbox_message;
    Tztest.tztest
      "Encode/decode outbox message"
      `Quick
      test_encode_decode_outbox_message;
  ]
