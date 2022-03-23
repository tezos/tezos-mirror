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
    Component:  Protocol (smart contract rollup inbox)
    Invocation: dune exec src/proto_alpha/lib_protocol/test/unit/main.exe \
                -- test "^\[Unit\] sc rollup inbox$"
    Subject:    These unit tests check the off-line inbox implementation for
                smart contract rollups
*)

open Protocol
open Sc_rollup_inbox_repr

exception Sc_rollup_inbox_test_error of string

let err x = Exn (Sc_rollup_inbox_test_error x)

let rollup = Sc_rollup_repr.Address.hash_string [""]

let level =
  Raw_level_repr.of_int32 0l |> function Ok x -> x | _ -> assert false

let create_context () =
  Context.init 1 >>=? fun (block, _) -> return block.context

let test_empty () =
  let inbox = empty rollup level in
  fail_unless
    Z.(equal (number_of_available_messages inbox) zero)
    (err "An empty inbox should have no available message.")

let setup_inbox_with_messages list_of_payloads f =
  create_context () >>=? fun ctxt ->
  let empty_messages = Environment.Context.Tree.empty ctxt in
  let inbox = empty rollup level in
  let history = history_at_genesis ~bound:10000L in
  let rec aux level history inbox inboxes messages = function
    | [] -> return (messages, history, inbox, inboxes)
    | payloads :: ps ->
        add_messages history inbox level payloads messages
        >>=? fun (messages, history, inbox') ->
        let level = Raw_level_repr.succ level in
        aux level history inbox' (inbox :: inboxes) messages ps
  in
  aux level history inbox [] empty_messages list_of_payloads
  >|= Environment.wrap_tzresult
  >>=? fun (messages, history, inbox, inboxes) ->
  f messages history inbox inboxes

let test_add_messages payloads =
  let nb_payloads = List.length payloads in
  setup_inbox_with_messages [payloads]
  @@ fun _messages _history inbox _inboxes ->
  fail_unless
    Z.(equal (number_of_available_messages inbox) (of_int nb_payloads))
    (err "Invalid number of available messages.")

let test_consume_messages (payloads, nb_consumed_messages) =
  let nb_payloads = List.length payloads in
  setup_inbox_with_messages [payloads]
  @@ fun _messages _history inbox _inboxes ->
  consume_n_messages nb_consumed_messages inbox |> Environment.wrap_tzresult
  >>?= function
  | Some inbox ->
      let available_messages = nb_payloads - nb_consumed_messages in
      fail_unless
        Z.(
          equal (number_of_available_messages inbox) (of_int available_messages))
        (err "Invalid number of available messages.")
  | None ->
      fail_unless
        (nb_consumed_messages > nb_payloads)
        (err
           "Message consumption fails only when trying to consume more than \
            the number of available messages.")

let check_payload message payload =
  Environment.Context.Tree.find message ["payload"] >>= function
  | None -> fail (err "No payload in message")
  | Some payload' ->
      let payload' = Bytes.to_string payload' in
      fail_unless
        (payload = payload')
        (err (Printf.sprintf "Expected payload %s, got %s" payload payload'))

let test_get_message payloads =
  setup_inbox_with_messages [payloads]
  @@ fun messages _history _inbox _inboxes ->
  List.iteri_es
    (fun i payload ->
      get_message messages (Z.of_int i) >>= function
      | Some message -> check_payload message payload
      | None -> fail (err (Printf.sprintf "No message number %d in messages" i)))
    payloads

let test_get_message_payload payloads =
  setup_inbox_with_messages [payloads]
  @@ fun messages _history _inbox _inboxes ->
  List.iteri_es
    (fun i payload ->
      get_message_payload messages (Z.of_int i) >>= function
      | Some payload' ->
          fail_unless
            (String.equal payload' payload)
            (err (Printf.sprintf "Expected %s, got %s" payload payload'))
      | None ->
          fail
            (err (Printf.sprintf "No message payload number %d in messages" i)))
    payloads

let test_inclusion_proof_production (list_of_payloads, n) =
  setup_inbox_with_messages list_of_payloads
  @@ fun _messages history _inbox inboxes ->
  let inbox = Stdlib.List.hd inboxes in
  let old_inbox = Stdlib.List.nth inboxes n in
  produce_inclusion_proof history old_inbox inbox |> function
  | None ->
      fail
      @@ err
           "It should be possible to produce an inclusion proof between two \
            versions of the same inbox."
  | Some proof ->
      fail_unless
        (verify_inclusion_proof proof old_inbox inbox)
        (err "The produced inclusion proof is invalid.")

let test_inclusion_proof_verification (list_of_payloads, n) =
  setup_inbox_with_messages list_of_payloads
  @@ fun _messages history _inbox inboxes ->
  let inbox = Stdlib.List.hd inboxes in
  let old_inbox = Stdlib.List.nth inboxes n in
  produce_inclusion_proof history old_inbox inbox |> function
  | None ->
      fail
      @@ err
           "It should be possible to produce an inclusion proof between two \
            versions of the same inbox."
  | Some proof ->
      let old_inbox' = Stdlib.List.nth inboxes (Random.int (1 + n)) in
      fail_unless
        (equal old_inbox old_inbox'
        || not (verify_inclusion_proof proof old_inbox' inbox))
        (err
           "Verification should rule out a valid proof which is not about the \
            given inboxes.")

let tests =
  [
    Tztest.tztest "Empty inbox" `Quick test_empty;
    Tztest.tztest_qcheck
      ~name:"Added messages are available."
      QCheck.(list string)
      test_add_messages;
    Tztest.tztest_qcheck
      ~name:"Get message."
      QCheck.(list string)
      test_get_message;
    Tztest.tztest_qcheck
      ~name:"Get message payload."
      QCheck.(list string)
      test_get_message_payload;
    Tztest.tztest_qcheck
      ~name:"Consume only available messages."
      QCheck.(
        make
          Gen.(
            let* l = list_size small_int string in
            let* n = 0 -- ((List.length l * 2) + 1) in
            return (l, n)))
      test_consume_messages;
  ]
  @
  let gen_inclusion_proof_inputs =
    QCheck.(
      make
        Gen.(
          let small = 2 -- 10 in
          let* a = list_size small string in
          let* b = list_size small string in
          let* l = list_size small (list_size small string) in
          let l = a :: b :: l in
          let* n = 0 -- (List.length l - 2) in
          return (l, n)))
  in
  [
    Tztest.tztest_qcheck
      ~count:10
      ~name:"Produce inclusion proof between two related inboxes."
      gen_inclusion_proof_inputs
      test_inclusion_proof_production;
    Tztest.tztest_qcheck
      ~count:10
      ~name:"Verify inclusion proofs."
      gen_inclusion_proof_inputs
      test_inclusion_proof_verification;
  ]
