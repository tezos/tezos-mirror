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

open Protocol.Alpha_context
open Injector_sigs

type tag =
  | Commitment
  | Submit_batch
  | Finalize_commitment
  | Remove_commitment
  | Rejection
  | Dispatch_withdrawals

module Parameters = struct
  type rollup_node_state = State.t

  let events_section = ["tx_rollup_node"; "injector"]

  module Tag = struct
    type t = tag

    let compare = Stdlib.compare

    let string_of_tag : t -> string = function
      | Submit_batch -> "submit_batch"
      | Commitment -> "commitment"
      | Finalize_commitment -> "finalize_commitment"
      | Remove_commitment -> "remove_commitment"
      | Rejection -> "rejection"
      | Dispatch_withdrawals -> "dispatch_withdrawals"

    let pp ppf t = Format.pp_print_string ppf (string_of_tag t)

    let encoding : t Data_encoding.t =
      let open Data_encoding in
      string_enum
        (List.map
           (fun t -> (string_of_tag t, t))
           [
             Submit_batch;
             Commitment;
             Finalize_commitment;
             Remove_commitment;
             Rejection;
             Dispatch_withdrawals;
           ])
  end

  (* Very coarse approximation for the number of operation we expect for each
     block *)
  let table_estimated_size = function
    | Commitment -> 3
    | Submit_batch -> 509
    | Finalize_commitment -> 3
    | Remove_commitment -> 3
    | Rejection -> 3
    | Dispatch_withdrawals -> 89

  let tag_operation (type kind) (op : kind manager_operation) =
    match op with
    | Tx_rollup_submit_batch _ -> Some Submit_batch
    | Tx_rollup_commit _ -> Some Commitment
    | Tx_rollup_finalize_commitment _ -> Some Finalize_commitment
    | Tx_rollup_remove_commitment _ -> Some Remove_commitment
    | Tx_rollup_rejection _ -> Some Rejection
    | Tx_rollup_dispatch_tickets _ -> Some Dispatch_withdrawals
    | _ -> None

  let fee_parameter (rollup_node_state : State.t) op =
    let Node_config.{fee_cap; burn_cap} =
      Option.fold
        (tag_operation op)
        ~none:Node_config.default_cost_caps
        ~some:(function
          | Commitment -> rollup_node_state.caps.operator
          | Submit_batch -> rollup_node_state.caps.submit_batch
          | Finalize_commitment -> rollup_node_state.caps.finalize_commitment
          | Remove_commitment -> rollup_node_state.caps.remove_commitment
          | Rejection -> rollup_node_state.caps.rejection
          | Dispatch_withdrawals -> rollup_node_state.caps.dispatch_withdrawals)
    in
    Injection.
      {
        minimal_fees = Tez.of_mutez_exn 100L;
        minimal_nanotez_per_byte = Q.of_int 1000;
        minimal_nanotez_per_gas_unit = Q.of_int 100;
        force_low_fee = false;
        fee_cap;
        burn_cap;
      }

  (* Below are dummy values that are only used to approximate the
     size. It is thus important that they remain above the real
     values if we want the computed size to be an over_approximation
     (without having to do a simulation first). *)
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/2812
     check the size, or compute them wrt operation kind *)
  let approximate_fee_bound _ =
    {
      fee = Tez.of_mutez_exn 3_000_000L;
      counter = Z.of_int 500_000;
      gas_limit = Gas.Arith.integral_of_int_exn 500_000;
      storage_limit = Z.of_int 500_000;
    }

  (* TODO: https://gitlab.com/tezos/tezos/-/issues/2813
     Decide if some operations must all succeed *)
  let batch_must_succeed _ = `At_least_one

  let ignore_failing_operation : type kind. kind manager_operation -> _ =
    function
    | Tx_rollup_remove_commitment _ | Tx_rollup_finalize_commitment _ ->
        (* We can keep these operations as there will be at most one of them in
           the queue at any given time. *)
        `Ignore_keep
    | _ -> `Don't_ignore

  (** Returns [true] if an included operation should be re-queued for injection
    when the block in which it is included is reverted (due to a
    reorganization). *)
  let requeue_reverted_operation (type kind) state
      (operation : kind manager_operation) =
    let open Lwt_syntax in
    match operation with
    | Tx_rollup_rejection _ ->
        (* TODO: check if rejected commitment in still in main chain *)
        return_true
    | Tx_rollup_commit {commitment; _} -> (
        let level = commitment.level in
        let* l2_block = State.get_level_l2_block state level in
        match l2_block with
        | None ->
            (* We don't know this L2 block, should not happen *)
            let+ () = Debug_events.(emit should_not_happen) __LOC__ in
            false
        | Some l2_block ->
            let commit_hash =
              Tx_rollup_commitment.(Compact.hash (Full.compact commitment))
            in
            (* Do not re-queue if commitment for this level has changed *)
            return
              Tx_rollup_commitment_hash.(
                l2_block.L2block.header.commitment = commit_hash))
    | _ -> return_true
end

include Injector_functor.Make (Parameters)
