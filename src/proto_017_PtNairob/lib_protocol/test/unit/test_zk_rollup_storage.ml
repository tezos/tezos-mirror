(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>           *)
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
    Component:  Protocol (Zk_rollup)
    Invocation: cd src/proto_alpha/lib_protocol/test/unit && \
                dune exec ./main.exe -- test "^\[Unit\] zk rollup storage$"
    Subject:    On ZK Rollup storage
*)

open Protocol

let wrap e = Lwt.return (Environment.wrap_tzresult e)

let ( let** ) m f =
  let open Lwt_result_syntax in
  let* x = m >>= wrap in
  f x

let batch_size = 10

module ZKRU = struct
  include Alpha_context.Zk_rollup

  type pkh = Signature.Public_key_hash.t

  let pkh_encoding = Signature.Public_key_hash.encoding

  type ticket_hash = Alpha_context.Ticket_hash.t

  let ticket_hash_encoding = Alpha_context.Ticket_hash.encoding
end

module Operator = Dummy_zk_rollup.Operator (struct
  let batch_size = batch_size
end)

let no_ticket op = (op, None)

module Raw_context_tests = struct
  module Helpers = struct
    let is_empty : Zk_rollup_repr.pending_list -> bool = function
      | Zk_rollup_repr.Empty _ -> true
      | _ -> false

    let pending_length : Zk_rollup_repr.pending_list -> int =
      let open Zk_rollup_repr in
      function Empty _ -> 0 | Pending {length; _} -> length

    let get_pending_list =
      let open Lwt_result_syntax in
      let open Zk_rollup_repr in
      fun ctx rollup -> function
        | Empty _ -> return (ctx, [])
        | Pending {next_index; length} ->
            let head = Int64.(sub next_index (of_int length)) in
            let to_get =
              Stdlib.List.init length (fun x -> Int64.(add (of_int x) head))
            in
            let* ctx, ops =
              List.fold_left_es
                (fun (ctx, acc) i ->
                  let** ctx, op =
                    Storage.Zk_rollup.Pending_operation.get (ctx, rollup) i
                  in
                  return (ctx, op :: acc))
                (ctx, [])
                to_get
            in
            return (ctx, List.rev ops)
  end

  let initial_ctx () =
    let open Lwt_result_syntax in
    let* b, contract = Context.init1 () in
    let** ctx =
      Raw_context.prepare
        b.context
        ~level:b.header.shell.level
        ~predecessor_timestamp:b.header.shell.timestamp
        ~timestamp:b.header.shell.timestamp
    in
    let nonce = Operation_hash.hash_string ["nonce_hash"] in
    return (Raw_context.init_origination_nonce ctx nonce, contract)

  (* Context with an originated ZKRU *)
  let originate_ctx () =
    let open Lwt_result_syntax in
    let open Zk_rollup_account_repr in
    let* ctx, contract = initial_ctx () in
    let _prover_pp, public_parameters = Lazy.force Operator.lazy_pp in
    let state = Operator.init_state in
    let state_length = Array.length state in
    let circuits_info = SMap.of_seq @@ Plonk.SMap.to_seq Operator.circuits in
    let nb_ops = 1 in
    let* ctx, rollup, _size =
      Zk_rollup_storage.originate
        ctx
        {public_parameters; state_length; circuits_info; nb_ops}
        ~init_state:state
      >>= wrap
    in
    return (ctx, rollup, contract)

  (* Check that the pending list of a new ZKRU is empty *)
  let pending_list_origination_is_empty () =
    let open Lwt_result_syntax in
    let* ctx, rollup, _contract = originate_ctx () in
    let** _ctx, pending = Storage.Zk_rollup.Pending_list.get ctx rollup in
    assert (Helpers.is_empty pending) ;
    return_unit

  (* Check that appending an L2 operation with the [add_to_pending] helper
     correctly updates both the pending list descriptor and the actual
     operations under the [pending_operations] directory. *)
  let pending_list_append () =
    let open Lwt_result_syntax in
    let* ctx, rollup, _contract = originate_ctx () in
    let pkh, _, _ = Signature.generate_key () in
    let op =
      no_ticket
        Zk_rollup_operation_repr.
          {
            op_code = 0;
            price = {id = Ticket_hash_repr.zero; amount = Z.zero};
            l1_dst = pkh;
            rollup_id = rollup;
            payload = [||];
          }
    in
    (* Append first operation *)
    let** ctx, _size = Zk_rollup_storage.add_to_pending ctx rollup [op] in
    let** ctx, pending = Storage.Zk_rollup.Pending_list.get ctx rollup in
    assert (Helpers.pending_length pending = 1) ;
    let* ctx, ops = Helpers.get_pending_list ctx rollup pending in
    assert (List.length ops = 1) ;
    (* Append second operation *)
    let** ctx, _size = Zk_rollup_storage.add_to_pending ctx rollup [op] in
    let** ctx, pending = Storage.Zk_rollup.Pending_list.get ctx rollup in
    let* _ctx, ops = Helpers.get_pending_list ctx rollup pending in
    assert (Helpers.pending_length pending = 2) ;
    assert (List.length ops = 2) ;
    return_unit

  let pending_list_append_errors () =
    let open Lwt_result_syntax in
    let* ctx, rollup, _contract = originate_ctx () in
    let pkh, _, _ = Signature.generate_key () in
    let op =
      no_ticket
        Zk_rollup_operation_repr.
          {
            op_code = 0;
            price = {id = Ticket_hash_repr.zero; amount = Z.zero};
            l1_dst = pkh;
            rollup_id = rollup;
            payload = [||];
          }
    in
    (* Append first operation *)
    let** ctx, _size = Zk_rollup_storage.add_to_pending ctx rollup [op] in
    let** ctx, pending = Storage.Zk_rollup.Pending_list.get ctx rollup in
    assert (Helpers.pending_length pending = 1) ;
    let* ctx, ops = Helpers.get_pending_list ctx rollup pending in
    assert (List.length ops = 1) ;
    (* Invalid op code *)
    let wrong_op =
      no_ticket
        Zk_rollup_operation_repr.
          {
            op_code = 1;
            price = {id = Ticket_hash_repr.zero; amount = Z.zero};
            l1_dst = pkh;
            rollup_id = rollup;
            payload = [||];
          }
    in
    let*! e = Zk_rollup_storage.add_to_pending ctx rollup [wrong_op] >>= wrap in
    let* () =
      Assert.proto_error_with_info ~loc:__LOC__ e "Invalid op code in append"
    in
    (* Invalid rollup address *)
    let* _ctx, nonce = Raw_context.increment_origination_nonce ctx |> wrap in
    let* address =
      Zk_rollup_repr.Address.from_nonce (Origination_nonce.incr nonce) |> wrap
    in
    let*! e = Zk_rollup_storage.add_to_pending ctx address [op] >>= wrap in
    let expected_message = "Storage error (fatal internal error)" in
    Assert.proto_error_with_info ~loc:__LOC__ e expected_message

  (* Check that the [get_prefix] helper actually returns a list of the
     desired length. *)
  let pending_list_get () =
    let open Lwt_result_syntax in
    let* ctx, rollup, _contract = originate_ctx () in
    let pkh, _pk, _sk = Signature.generate_key () in
    let op =
      no_ticket
        Zk_rollup_operation_repr.
          {
            op_code = 0;
            price = {id = Ticket_hash_repr.zero; amount = Z.zero};
            l1_dst = pkh;
            rollup_id = rollup;
            payload = [|Bls12_381.Fr.one|];
          }
    in
    let** ctx, _size = Zk_rollup_storage.add_to_pending ctx rollup [op; op] in
    let** _ctx, prefix = Zk_rollup_storage.get_prefix ctx rollup 1 in
    assert (List.length prefix = 1) ;
    return_unit

  (* Check the [get_prefix] errors. *)
  let pending_list_errors () =
    let open Lwt_result_syntax in
    let* ctx, rollup, _contract = originate_ctx () in
    let pkh, _pk, _sk = Signature.generate_key () in
    let op =
      no_ticket
        Zk_rollup_operation_repr.
          {
            op_code = 0;
            price = {id = Ticket_hash_repr.zero; amount = Z.zero};
            l1_dst = pkh;
            rollup_id = rollup;
            payload = [|Bls12_381.Fr.one|];
          }
    in
    (* Initialise the pending list with 2 operations *)
    let** ctx, _size = Zk_rollup_storage.add_to_pending ctx rollup [op; op] in
    (* Check that retrieving too many ops returns an error *)
    let*! e = Zk_rollup_storage.get_prefix ctx rollup 3 >>= wrap in
    let* () =
      Assert.proto_error_with_info ~loc:__LOC__ e "Pending list is too short"
    in
    (* Check that retrieving a negative number of ops returns an error *)
    let*! e = Zk_rollup_storage.get_prefix ctx rollup (-1) >>= wrap in
    let* () =
      Assert.proto_error_with_info
        ~loc:__LOC__
        e
        "Negative length for pending list prefix"
    in
    (* Check that get prefix fails with invalid zkru address *)
    let* _ctx, nonce = Raw_context.increment_origination_nonce ctx |> wrap in
    let* address =
      Zk_rollup_repr.Address.from_nonce (Origination_nonce.incr nonce) |> wrap
    in
    let*! e = Zk_rollup_storage.get_prefix ctx address (-1) >>= wrap in
    Assert.proto_error_with_info
      ~loc:__LOC__
      e
      "Storage error (fatal internal error)"

  (* Check that the [update] helper correctly removes a prefix of the
     pending list (both in the descriptor and the actual operations storage).
  *)
  let test_update () =
    let open Lwt_result_syntax in
    (* Originate rollup and contract *)
    let* ctx, rollup, contract = originate_ctx () in
    let pkh =
      match contract with Originated _ -> assert false | Implicit pkh -> pkh
    in
    let op =
      no_ticket
        Zk_rollup_operation_repr.
          {
            op_code = 0;
            price = {id = Ticket_hash_repr.zero; amount = Z.zero};
            l1_dst = pkh;
            rollup_id = rollup;
            payload = [|Bls12_381.Fr.one|];
          }
    in
    (* Populate rollup with 2 ops *)
    let** ctx, _size = Zk_rollup_storage.add_to_pending ctx rollup [op; op] in
    let** ctx, acc = Storage.Zk_rollup.Account.get ctx rollup in
    (* Processing first pending op *)
    let** ctx =
      Zk_rollup_storage.update ctx rollup ~pending_to_drop:1 ~new_account:acc
    in
    (* Check that op at index 0 has been removed *)
    let** ctx, opt =
      Storage.Zk_rollup.Pending_operation.find (ctx, rollup) 0L
    in
    assert (Option.is_none opt) ;
    (* Check that pending list still has one op *)
    let** ctx, pending = Storage.Zk_rollup.Pending_list.get ctx rollup in
    assert (Helpers.pending_length pending = 1) ;
    let* _ctx, ops = Helpers.get_pending_list ctx rollup pending in
    assert (List.length ops = 1) ;
    return_unit

  let test_update_errors () =
    let open Lwt_result_syntax in
    (* Originate rollup and contract *)
    let* ctx, rollup, contract = originate_ctx () in
    let pkh =
      match contract with Originated _ -> assert false | Implicit pkh -> pkh
    in
    let op =
      no_ticket
        Zk_rollup_operation_repr.
          {
            op_code = 0;
            price = {id = Ticket_hash_repr.zero; amount = Z.zero};
            l1_dst = pkh;
            rollup_id = rollup;
            payload = [|Bls12_381.Fr.one|];
          }
    in
    (* Populate rollup with 2 ops *)
    let** ctx, _size = Zk_rollup_storage.add_to_pending ctx rollup [op; op] in
    let** ctx, acc = Storage.Zk_rollup.Account.get ctx rollup in
    (* Processing too many ops *)
    let*! e =
      Zk_rollup_storage.update ctx rollup ~pending_to_drop:3 ~new_account:acc
      >>= wrap
    in
    let* () =
      Assert.proto_error_with_info ~loc:__LOC__ e "Pending list is too short"
    in
    (* Processing negative number of ops *)
    let*! e =
      Zk_rollup_storage.update ctx rollup ~pending_to_drop:(-3) ~new_account:acc
      >>= wrap
    in
    let* () =
      Assert.proto_error_with_info
        ~loc:__LOC__
        e
        "Negative length for pending list prefix"
    in
    (* Update with wrong address *)
    let* _ctx, nonce = Raw_context.increment_origination_nonce ctx |> wrap in
    let* address =
      Zk_rollup_repr.Address.from_nonce (Origination_nonce.incr nonce) |> wrap
    in
    let*! e =
      Zk_rollup_storage.update ctx address ~pending_to_drop:1 ~new_account:acc
      >>= wrap
    in
    Assert.proto_error_with_info
      ~loc:__LOC__
      e
      "Storage error (fatal internal error)"
end

let tests =
  [
    Tztest.tztest
      "origination_pending_is_empty"
      `Quick
      Raw_context_tests.pending_list_origination_is_empty;
    Tztest.tztest
      "pending_list_append"
      `Quick
      Raw_context_tests.pending_list_append;
    Tztest.tztest
      "pending_list_append errors"
      `Quick
      Raw_context_tests.pending_list_append_errors;
    Tztest.tztest "pending_list_get" `Quick Raw_context_tests.pending_list_get;
    Tztest.tztest
      "pending_list_get errors"
      `Quick
      Raw_context_tests.pending_list_errors;
    Tztest.tztest "test_update" `Quick Raw_context_tests.test_update;
    Tztest.tztest
      "test_update errors"
      `Quick
      Raw_context_tests.test_update_errors;
  ]
