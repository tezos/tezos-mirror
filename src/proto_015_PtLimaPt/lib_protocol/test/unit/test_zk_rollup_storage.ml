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
    Invocation: dune exec src/proto_015_PtLimaPt/lib_protocol/test/unit/main.exe
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

  type pkh = Tezos_crypto.Signature.V0.Public_key_hash.t

  let pkh_encoding = Tezos_crypto.Signature.V0.Public_key_hash.encoding

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
    let public_parameters = Operator.public_parameters in
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
    let pkh, _, _ = Tezos_crypto.Signature.V0.generate_key () in
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
    let pkh, _, _ = Tezos_crypto.Signature.V0.generate_key () in
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
    let*! _e = Zk_rollup_storage.add_to_pending ctx address [op] >>= wrap in
    return_unit
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
  ]
