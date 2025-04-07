(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

open Protocol
open Alpha_context
open Tezos_protocol_environment

type simulation_result = {
  validation_result : validation_result option;
  block_header_metadata : Apply_results.block_metadata option;
  operations : packed_operation list list;
  operations_hash : Operation_list_list_hash.t;
  manager_operations_infos : Baking_state.manager_operations_infos option;
}

(** [filter_operations_with_simulation incremental fees_config
    ~hard_gas_limit_per_block ops] tries to validate prioritized operations (and
    apply them if [incremental] has been initialised with an
    [application_state]) and filter them regarding the quota of each validation
    pass. Manager operations are prioritized based on a weight computed from
    their fees/gas/bytes. [filter_operations_with_simulation] function returns a
    [simulation_result], containing the validated operation, their resulting
    [operations_hash], optional [validation_result] and [block_header_metadata]
    if the operations were applied. *)
val filter_operations_with_simulation :
  Baking_simulator.incremental ->
  Baking_configuration.fees_config ->
  hard_gas_limit_per_block:Gas.Arith.integral ->
  Operation_pool.Prioritized.t ->
  simulation_result tzresult Lwt.t

(** [filter_operations_without_simulation fees_config ~hard_gas_limit_per_block
    ops] is similar to [filter_operations_with_simulation] but does not validate
    (and apply) operations from [ops] and returns only the operations instead of
    a [simulation_result].

    Hypothesis: operations from [ops] have previously been validated. *)
val filter_operations_without_simulation :
  Baking_configuration.fees_config ->
  hard_gas_limit_per_block:Gas.Arith.integral ->
  Operation_pool.Prioritized.t ->
  packed_operation list list

(** [filter_consensus_operations_only incremental ops] is similar to
    [filter_operations_with_simulation] but only filters consensus operations
    from [ops]. *)
val filter_consensus_operations_only :
  Baking_simulator.incremental ->
  Operation_pool.ordered_pool ->
  (Baking_simulator.incremental * Operation_pool.ordered_pool) tzresult Lwt.t
