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
  let*? bytes =
    Environment.wrap_tzresult @@ Sc_rollup.Inbox.Message.to_bytes message
  in
  let bytes = (bytes :> string) in
  let*? message' =
    Environment.wrap_tzresult @@ Internal_for_tests.inbox_message_of_bytes bytes
  in
  let*? bytes' =
    Environment.wrap_tzresult @@ Sc_rollup.Inbox.Message.to_bytes message'
  in
  let bytes' = (bytes' :> string) in
  Assert.equal_string ~loc:__LOC__ bytes bytes'

let check_encode_decode_outbox_message ctxt message =
  let open Lwt_result_syntax in
  let open Sc_rollup_management_protocol in
  let*? bytes =
    Environment.wrap_tzresult
    @@ Internal_for_tests.bytes_of_outbox_message message
  in
  let* message', _ctxt = wrap @@ outbox_message_of_bytes ctxt bytes in
  let*? bytes' =
    Environment.wrap_tzresult
    @@ Internal_for_tests.bytes_of_outbox_message message'
  in
  Assert.equal_string ~loc:__LOC__ bytes bytes'

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
  let* block, _baker, _contract, _src2 = Contract_helpers.init () in
  let+ incr = Incremental.begin_construction block in
  Incremental.alpha_ctxt incr

let test_encode_decode_internal_inbox_message () =
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
  let* deposit, _ctxt =
    wrap
    @@ Sc_rollup_management_protocol.make_internal_inbox_message
         ctxt
         pair_nat_ticket_string_ty
         ~payload
         ~sender
         ~source
  in
  check_encode_decode_inbox_message deposit

let test_encode_decode_external_inbox_message () =
  let open Lwt_result_syntax in
  let assert_prefix message ~prefix =
    let inbox_message = Sc_rollup.Inbox.Message.External message in
    let*? real_encoding =
      Environment.wrap_tzresult
      @@ Sc_rollup.Inbox.Message.to_bytes inbox_message
    in
    let real_encoding = (real_encoding :> string) in
    (* The prefix consists of 5 bytes:
       - Byte 0 is the tag (0 for internal, 1 for external).
       - Bytes 1-4 is the length of the message encoded as:
          [ prefix[1] * 256^3 + prefix[2] * 256^2 prefix[3] * 256^1 prefix[4]]
    *)
    let real_prefix =
      String.sub real_encoding 0 5
      |> String.to_seq |> List.of_seq |> List.map Char.code
    in
    let expected_encoding =
      Printf.sprintf
        "%s%s"
        (List.map Char.chr prefix |> List.to_seq |> String.of_seq)
        message
    in
    (* Check that the encode/decode matches. *)
    let* () = check_encode_decode_inbox_message inbox_message in
    (* Check that the prefix match. *)
    let* () =
      Assert.assert_equal_list
        ~loc:__LOC__
        Int.equal
        "Compare encoded prefix"
        Format.pp_print_int
        real_prefix
        prefix
    in
    (* Check that the encoded string consists of the prefix followed by the
       original message. *)
    Assert.equal_string ~loc:__LOC__ real_encoding expected_encoding
  in
  let* () = assert_prefix "" ~prefix:[1; 0; 0; 0; 0] in
  let* () = assert_prefix "A" ~prefix:[1; 0; 0; 0; 1] in
  let* () = assert_prefix "0123456789" ~prefix:[1; 0; 0; 0; 10] in
  let* () =
    assert_prefix (String.init 256 (Fun.const 'A')) ~prefix:[1; 0; 0; 1; 0]
  in
  (* Length 1234567 = 18*256^2 + 214*256 + 135 *)
  let* () =
    assert_prefix
      (String.init 1234567 (Fun.const 'A'))
      ~prefix:[1; 0; 18; 214; 135]
  in
  (* The content of the string should not impact the prefix.*)
  let* () =
    assert_prefix
      (String.init 1234567 (Fun.const 'b'))
      ~prefix:[1; 0; 18; 214; 135]
  in
  return_unit

let init_env () =
  let open Lwt_result_syntax in
  let* block, baker, contract, _src2 = Contract_helpers.init () in
  return (block, baker, contract)

let ticket_receiver =
  {|
      { parameter (pair nat (ticket string));
        storage (list (ticket string));
        code { UNPAIR;          # [(nat, ticket) ; list]
               CDR;             # [ticket ; list]
               CONS;            # [ticket :: list]
               NIL operation ;  # [[] ;  ticket :: list]
               PAIR;            # [([], ticket :: list)]
              }
      }
    |}

let add_or_clear =
  {|
    { parameter (or (pair %add nat string) (unit %clear)) ;
      storage (list (ticket string)) ;
      code { UNPAIR ;
            IF_LEFT
            { UNPAIR ; DIG 2 ; SWAP ; DIG 2 ; TICKET ; CONS ; NIL operation ; PAIR }
            { DROP 2 ; NIL (ticket string) ; NIL operation ; PAIR } } }
  |}

let test_encode_decode_outbox_message () =
  let open Lwt_result_syntax in
  let* block, baker, source_contract = init_env () in
  let* ticket_receiver, _, block =
    Contract_helpers.originate_contract_from_string
      ~script:ticket_receiver
      ~storage:"{}"
      ~source_contract
      ~baker
      block
  in
  let* add_or_clear, _, block =
    Contract_helpers.originate_contract_from_string
      ~script:add_or_clear
      ~storage:"{}"
      ~source_contract
      ~baker
      block
  in
  let* incr = Incremental.begin_construction block in
  let ctxt = Incremental.alpha_ctxt incr in
  let ticket_receiver_destination =
    match ticket_receiver with
    | Contract.Originated ch -> ch
    | Implicit _ -> assert false
  in
  let add_or_clear_destination =
    match add_or_clear with
    | Contract.Originated ch -> ch
    | Implicit _ -> assert false
  in
  (* Transaction to ticket receiver. *)
  let* transaction1, ctxt =
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
    wrap
    @@ Sc_rollup_management_protocol.Internal_for_tests.make_transaction
         ctxt
         pair_nat_ticket_string_ty
         ~parameters
         ~destination:ticket_receiver_destination
         ~entrypoint:Entrypoint.default
  in
  (* Transaction to the `add` endpoint of add-or-clear contract. *)
  let* transaction2, ctxt =
    let*? (Script_typed_ir.Ty_ex_c pair_nat_ticket_string_ty) =
      Environment.wrap_tzresult Script_typed_ir.(pair_t (-1) nat_t string_t)
    in
    let*? content =
      Environment.wrap_tzresult @@ Script_string.of_string "Hello"
    in
    let parameters = (Script_int.(abs @@ of_int 11), content) in
    wrap
    @@ Sc_rollup_management_protocol.Internal_for_tests.make_transaction
         ctxt
         pair_nat_ticket_string_ty
         ~parameters
         ~destination:add_or_clear_destination
         ~entrypoint:(Entrypoint.of_string_strict_exn "add")
  in
  (* Transaction to the `clear` endpoint of add-or-clear contract. *)
  let* transaction3, ctxt =
    wrap
    @@ Sc_rollup_management_protocol.Internal_for_tests.make_transaction
         ctxt
         Script_typed_ir.unit_t
         ~parameters:()
         ~destination:add_or_clear_destination
         ~entrypoint:(Entrypoint.of_string_strict_exn "clear")
  in
  let outbox_message =
    Sc_rollup_management_protocol.Internal_for_tests.make_atomic_batch
      [transaction1; transaction2; transaction3]
  in
  check_encode_decode_outbox_message ctxt outbox_message

let tests =
  [
    Tztest.tztest
      "Encode/decode internal inbox message"
      `Quick
      test_encode_decode_internal_inbox_message;
    Tztest.tztest
      "Encode/decode external inbox message"
      `Quick
      test_encode_decode_external_inbox_message;
    Tztest.tztest
      "Encode/decode outbox message"
      `Quick
      test_encode_decode_outbox_message;
  ]
