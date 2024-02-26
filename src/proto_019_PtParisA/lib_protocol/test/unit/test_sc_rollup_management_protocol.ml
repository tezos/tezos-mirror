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
    Invocation: dune exec src/proto_019_PtParisA/lib_protocol/test/unit/main.exe \
                  -- --file test_sc_rollup_management_protocol.ml
    Subject:    Sanity checks for the Rollup Management Protocol module.
*)

open Protocol
open Alpha_context

let check_encode_decode_inbox_message message =
  let open Lwt_result_wrap_syntax in
  let open Sc_rollup_management_protocol in
  let*?@ bytes = Sc_rollup.Inbox_message.serialize message in
  let*?@ message' = Internal_for_tests.deserialize_inbox_message bytes in
  let*?@ bytes' = Sc_rollup.Inbox_message.serialize message' in
  Assert.equal_string
    ~loc:__LOC__
    (Sc_rollup.Inbox_message.unsafe_to_string bytes)
    (Sc_rollup.Inbox_message.unsafe_to_string bytes')

let check_encode_decode_outbox_message_untyped ctxt transactions =
  let open Lwt_result_wrap_syntax in
  let open Sc_rollup_management_protocol in
  let*?@ bytes =
    Internal_for_tests.serialize_outbox_transactions_untyped transactions
  in
  let*?@ message_repr = Sc_rollup.Outbox.Message.deserialize bytes in
  let*@ message', _ctxt =
    outbox_message_of_outbox_message_repr ctxt message_repr
  in
  let*?@ bytes' =
    match message' with
    | Whitelist_update _ -> assert false (* its serialized transaction *)
    | Atomic_transaction_batch {transactions} ->
        Internal_for_tests.serialize_outbox_transactions_untyped transactions
  in
  Assert.equal_string
    ~loc:__LOC__
    (Sc_rollup.Outbox.Message.unsafe_to_string bytes)
    (Sc_rollup.Outbox.Message.unsafe_to_string bytes')

let check_encode_decode_outbox_message_typed ctxt transactions =
  let open Lwt_result_wrap_syntax in
  let open Sc_rollup_management_protocol in
  let*?@ bytes =
    Internal_for_tests.serialize_outbox_transactions_typed transactions
  in
  let*?@ message_repr = Sc_rollup.Outbox.Message.deserialize bytes in
  let*@ message', _ctxt =
    outbox_message_of_outbox_message_repr ctxt message_repr
  in
  let*?@ bytes' =
    match message' with
    | Whitelist_update _ -> assert false (* its serialized transaction *)
    | Atomic_transaction_batch {transactions} ->
        Internal_for_tests.serialize_outbox_transactions_typed transactions
  in
  Assert.equal_string
    ~loc:__LOC__
    (Sc_rollup.Outbox.Message.unsafe_to_string bytes)
    (Sc_rollup.Outbox.Message.unsafe_to_string bytes')

let string_ticket ticketer contents amount =
  let open WithExceptions in
  let amount =
    Option.get ~loc:__LOC__ @@ Ticket_amount.of_n @@ Script_int.abs
    @@ Script_int.of_int amount
  in
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

let assert_encoding_failure ~loc res =
  Assert.proto_error_with_info
    ~loc
    res
    "Failed to encode a rollup management protocol inbox message value"

let test_encode_decode_internal_inbox_message_transfer () =
  let open Lwt_result_wrap_syntax in
  let open WithExceptions in
  let* ctxt = init_ctxt () in
  let destination = Sc_rollup.Address.zero in
  let sender =
    Contract_hash.of_b58check_exn "KT1BuEZtb68c1Q4yjtckcNjGELqWt56Xyesc"
  in
  let source =
    Result.get_ok
      ~loc:__LOC__
      (Signature.Public_key_hash.of_b58check
         "tz1RjtZUVeLhADFHDL8UwDZA6vjWWhojpu5w")
  in
  let*?@ (Script_typed_ir.Ty_ex_c pair_nat_ticket_string_ty) =
    let open Result_syntax in
    let open Script_typed_ir in
    let* ticket_t = ticket_t (-1) string_t in
    pair_t (-1) nat_t ticket_t
  in
  let payload =
    ( Script_int.(abs @@ of_int 42),
      string_ticket "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "red" 1 )
  in
  let*@ transfer, ctxt =
    Sc_rollup_management_protocol.make_internal_transfer
      ctxt
      pair_nat_ticket_string_ty
      ~payload
      ~sender
      ~source
      ~destination
  in
  let* () = check_encode_decode_inbox_message transfer in
  (* Check that the size of messages that can be encoded is bounded. *)
  let msg = String.make 4050 'c' in
  let*?@ payload = Script_string.of_string msg in
  let*@ transfer, _ctxt =
    let open Script_typed_ir in
    Sc_rollup_management_protocol.make_internal_transfer
      ctxt
      String_t
      ~payload
      ~sender
      ~source
      ~destination
  in
  let*! res = check_encode_decode_inbox_message transfer in
  assert_encoding_failure ~loc:__LOC__ res

let test_encode_decode_internal_inbox_message_sol () =
  let sol = Sc_rollup.Inbox_message.(Internal Start_of_level) in
  check_encode_decode_inbox_message sol

let test_encode_decode_internal_inbox_message_eol () =
  let eol = Sc_rollup.Inbox_message.(Internal End_of_level) in
  check_encode_decode_inbox_message eol

let test_encode_decode_external_inbox_message () =
  let open Lwt_result_wrap_syntax in
  let assert_prefix message =
    let inbox_message = Sc_rollup.Inbox_message.External message in
    let*?@ real_encoding = Sc_rollup.Inbox_message.serialize inbox_message in
    let real_encoding =
      Sc_rollup.Inbox_message.unsafe_to_string real_encoding
    in
    (* The prefix consists of a tag (0 for internal, 1 for external). *)
    let real_prefix = String.get real_encoding 0 in
    let expected_prefix = '\001' in
    let expected_encoding = Printf.sprintf "%c%s" expected_prefix message in
    (* Check that the encode/decode matches. *)
    let* () = check_encode_decode_inbox_message inbox_message in
    (* Check that the prefix match. *)
    let* () = Assert.equal_char ~loc:__LOC__ real_prefix expected_prefix in
    (* Check that the encoded string consists of the prefix followed by the
       original message. *)
    Assert.equal_string ~loc:__LOC__ real_encoding expected_encoding
  in
  let* () = assert_prefix "" in
  let* () = assert_prefix "A" in
  let* () = assert_prefix "0123456789" in
  let* () = assert_prefix (String.init 256 (Fun.const 'A')) in
  let assert_encoding_success message =
    let inbox_message = Sc_rollup.Inbox_message.External message in
    let*! res = check_encode_decode_inbox_message inbox_message in
    assert (Result.is_ok res) ;
    return_unit
  in
  let assert_encoding_failure message =
    let inbox_message = Sc_rollup.Inbox_message.External message in
    let*! res = check_encode_decode_inbox_message inbox_message in
    assert_encoding_failure ~loc:__LOC__ res
  in
  let max_msg_size = Constants_repr.sc_rollup_message_size_limit in
  let message = String.init (max_msg_size - 1) (Fun.const 'A') in
  let* () = assert_encoding_success message in
  let message = String.init max_msg_size (Fun.const 'b') in
  let* () = assert_encoding_failure message in
  assert_encoding_failure message

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
            { UNPAIR ; DIG 2 ; SWAP ; DIG 2 ; TICKET ; ASSERT_SOME ; CONS ; NIL operation ; PAIR }
            { DROP 2 ; NIL (ticket string) ; NIL operation ; PAIR } } }
  |}

let test_encode_decode_outbox_message () =
  let open Lwt_result_wrap_syntax in
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
  let*?@ (Script_typed_ir.Ty_ex_c pair_nat_ticket_string_ty) =
    let open Result_syntax in
    let open Script_typed_ir in
    let* ticket_t = ticket_t (-1) string_t in
    pair_t (-1) nat_t ticket_t
  in
  let parameters =
    ( Script_int.(abs @@ of_int 42),
      string_ticket "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" "red" 1 )
  in
  let*@ transaction1, ctxt =
    Sc_rollup_management_protocol.Internal_for_tests.make_transaction
      ctxt
      pair_nat_ticket_string_ty
      ~parameters
      ~destination:ticket_receiver_destination
      ~entrypoint:Entrypoint.default
  in
  (* Transaction to the `add` endpoint of add-or-clear contract. *)
  let*@ transaction2, ctxt =
    let*? (Script_typed_ir.Ty_ex_c pair_nat_ticket_string_ty) =
      Script_typed_ir.(pair_t (-1) nat_t string_t)
    in
    let*? content = Script_string.of_string "Hello" in
    let parameters = (Script_int.(abs @@ of_int 11), content) in
    Sc_rollup_management_protocol.Internal_for_tests.make_transaction
      ctxt
      pair_nat_ticket_string_ty
      ~parameters
      ~destination:add_or_clear_destination
      ~entrypoint:(Entrypoint.of_string_strict_exn "add")
  in
  (* Transaction to the `clear` endpoint of add-or-clear contract. *)
  let*@ transaction3, ctxt =
    Sc_rollup_management_protocol.Internal_for_tests.make_transaction
      ctxt
      Script_typed_ir.unit_t
      ~parameters:()
      ~destination:add_or_clear_destination
      ~entrypoint:(Entrypoint.of_string_strict_exn "clear")
  in
  let outbox_message = [transaction1; transaction2; transaction3] in
  let* () = check_encode_decode_outbox_message_untyped ctxt outbox_message in
  check_encode_decode_outbox_message_typed ctxt outbox_message

let test_whitelist_encoding_size () =
  let open Lwt_result_syntax in
  let gen_pkh () =
    let account = Account.new_account () in
    account.pkh
  in
  (* 6 is for the rest of encoding: size, tag, ... *)
  let max_msg_size = Constants_repr.sc_rollup_message_size_limit - 6 in
  let encoding_size =
    Data_encoding.Binary.length Signature.Public_key_hash.encoding (gen_pkh ())
  in
  let max_allowed_whitelist_size = max_msg_size / encoding_size in
  let check_encoding ~valid encoding msg =
    let res = Data_encoding.Binary.to_bytes encoding msg in
    match res with
    | Error _ when valid -> fail @@ [error_of_fmt "valid encoding is invalid"]
    | Ok _ when not valid -> fail @@ [error_of_fmt "invalid encoding is valid"]
    | _ -> return_unit
  in
  let*? max_whitelist =
    List.init
      ~when_negative_length:[error_of_fmt "invalid size"]
      max_allowed_whitelist_size
      (fun _i -> gen_pkh ())
  in
  let* () =
    let* () =
      check_encoding ~valid:true Sc_rollup.Whitelist.encoding max_whitelist
    in
    Sc_rollup.Outbox.Message.(
      check_encoding
        ~valid:true
        encoding
        (Whitelist_update (Some max_whitelist)))
  in
  let* () =
    let too_big_whitelist = gen_pkh () :: max_whitelist in
    let* () =
      check_encoding ~valid:false Sc_rollup.Whitelist.encoding too_big_whitelist
    in
    Sc_rollup.Outbox.Message.(
      check_encoding
        ~valid:false
        encoding
        (Whitelist_update (Some too_big_whitelist)))
  in
  return_unit

let tests =
  [
    Tztest.tztest
      "Encode/decode internal inbox message transfer"
      `Quick
      test_encode_decode_internal_inbox_message_transfer;
    Tztest.tztest
      "Encode/decode internal inbox message start of level"
      `Quick
      test_encode_decode_internal_inbox_message_sol;
    Tztest.tztest
      "Encode/decode internal inbox message end of level"
      `Quick
      test_encode_decode_internal_inbox_message_eol;
    Tztest.tztest
      "Encode/decode external inbox message"
      `Quick
      test_encode_decode_external_inbox_message;
    Tztest.tztest
      "Encode/decode outbox message"
      `Quick
      test_encode_decode_outbox_message;
    Tztest.tztest
      "Whitelist size is borned by max_msg_size"
      `Quick
      test_whitelist_encoding_size;
  ]

let () =
  Alcotest_lwt.run
    ~__FILE__
    Protocol.name
    [("sc rollup management protocol", tests)]
  |> Lwt_main.run
