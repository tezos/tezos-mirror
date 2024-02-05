(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Functori, <contact@functori.com>                       *)
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

open Injector_sigs

type state = {
  cctxt : Client_context.full;
  fee_parameters : Operation_kind.fee_parameters;
  minimal_block_delay : int64;
  delay_increment_per_round : int64;
}

module Parameters :
  PARAMETERS
    with type state = state
     and type Tag.t = Operation_kind.t
     and type Operation.t = L1_operation.t = struct
  type nonrec state = state

  let events_section = ["smart_rollup_node"]

  module Tag : TAG with type t = Operation_kind.t = struct
    type t = Operation_kind.t

    let compare = Stdlib.compare

    let equal = Stdlib.( = )

    let hash = Hashtbl.hash

    let string_of_tag = Operation_kind.to_string

    let pp ppf t = Format.pp_print_string ppf (string_of_tag t)

    let encoding : t Data_encoding.t = Operation_kind.encoding
  end

  module Operation = L1_operation

  (* TODO: https://gitlab.com/tezos/tezos/-/issues/3459
     Very coarse approximation for the number of operation we
     expect for each block *)
  let table_estimated_size : Tag.t -> int = function
    | Publish -> 1
    | Add_messages -> 100
    | Cement -> 1
    | Timeout -> 1
    | Refute -> 1
    | Recover -> 1
    | Execute_outbox_message -> 1

  let operation_tag : Operation.t -> Tag.t = function
    | Add_messages _ -> Add_messages
    | Cement _ -> Cement
    | Publish _ -> Publish
    | Timeout _ -> Timeout
    | Refute _ -> Refute
    | Recover_bond _ -> Recover
    | Execute_outbox_message _ -> Execute_outbox_message

  let fee_parameter {fee_parameters; _} operation =
    let operation_kind = operation_tag operation in
    Operation_kind.Map.find operation_kind fee_parameters
    |> Option.value
         ~default:(Configuration.default_fee_parameter operation_kind)

  let safety_guard = function
    | Operation.Publish _ ->
        (* Gas consumption of commitment publication can increase if there are
           already commitments for the same level in the context. The value 300
           was inferred empirically by looking at gas consumption on a running
           rollup, and is sufficient to cover the difference in the case where a
           commitment for the same level already exists. *)
        Some 300
    | Timeout _ | Refute _ ->
        (* We increase safety of refutation game operations by precaution. *)
        Some 300
    | Add_messages _ | Cement _ | Recover_bond _ | Execute_outbox_message _ ->
        None

  (* TODO: https://gitlab.com/tezos/tezos/-/issues/3459
     Decide if some batches must have all the operations succeed. See
     {!Injector_sigs.Parameter.batch_must_succeed}. *)
  let batch_must_succeed _ = `At_least_one

  let retry_unsuccessful_operation _state (_op : Operation.t) status =
    let open Lwt_syntax in
    match status with
    | Backtracked | Skipped | Other_branch ->
        (* Always retry backtracked or skipped operations, or operations that
           are on another branch because of a reorg:

           - Commitments are always produced on finalized blocks. They don't
             need to be recomputed, and as such are valid in another branch.

           - The cementation operations should be re-injected because the node
             only keeps track of the last cemented level and the last published
             commitment, without rollbacks.

           - Messages posted to an inbox should be re-emitted (i.e. re-queued)
             in case of a fork.

           - Timeout should be re-submitted as the timeout may be reached as well
             on the other branch.

           - Refutation should be re-submitted in case of fork.
             TODO: https://gitlab.com/tezos/tezos/-/issues/3459
             maybe check if game exists on other branch as well.
        *)
        return Retry
    | Failed error -> (
        (* TODO: https://gitlab.com/tezos/tezos/-/issues/4071
           Think about which operations should be retried and when. *)
        match classify_trace error with
        | Permanent | Outdated -> return Forget
        | Branch | Temporary -> return Retry)
    | Never_included ->
        (* Forget operations that are never included *)
        return Forget
end

include Injector_functor.Make (Parameters)

let check_and_add_pending_operation (mode : Configuration.mode)
    (operation : L1_operation.t) =
  let open Lwt_result_syntax in
  if Configuration.(can_inject mode (Parameters.operation_tag operation)) then
    let* hash = add_pending_operation operation in
    return (Some hash)
  else return None
