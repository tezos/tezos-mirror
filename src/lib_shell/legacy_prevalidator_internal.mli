(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018-2022 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

(* FIXME: https://gitlab.com/tezos/tezos/-/issues/4113

   This file is part of the implementation of the legacy mempool,
   which is compatible with Kathmandu and therefore usable on Mainnet.

   This file should be removed once Lima has been activated on Mainnet.

   When you modify this file, consider whether you should also change
   the files that implement the more recent mempool for Lima and newer
   protocols. *)

(** Create a prevalidator instance for a specific protocol
    ([Filter.Proto] where [module Filter : Shell_plugin.FILTER]).

    The protocol must be Kathmandu (environment V6) or an older
    version. For more recent protocols, use
    {!Prevalidator_internal.make} instead.

    This function is wrapped in {!Prevalidator.create}. *)
val make :
  Shell_limits.prevalidator_limits ->
  Distributed_db.chain_db ->
  Tezos_crypto.Chain_id.t ->
  (module Legacy_mempool_plugin.FILTER) ->
  Prevalidator_internal_common.t

(**/**)

module Internal_for_tests : sig
  (** Documented in the ml file, because this is only exported for tests. *)
  type 'prevalidation_t tools = {
    advertise_current_head : mempool:Mempool.t -> Store.Block.t -> unit;
    chain_tools : Store.Block.t Legacy_prevalidator_classification.chain_tools;
    create :
      predecessor:Store.Block.t ->
      live_operations:Tezos_crypto.Operation_hash.Set.t ->
      timestamp:Time.Protocol.t ->
      unit ->
      'prevalidation_t tzresult Lwt.t;
    fetch :
      ?peer:P2p_peer.Id.t ->
      ?timeout:Time.System.Span.t ->
      Tezos_crypto.Operation_hash.t ->
      Operation.t tzresult Lwt.t;
    read_block : Tezos_crypto.Block_hash.t -> Store.Block.t tzresult Lwt.t;
    send_get_current_head : ?peer:P2p_peer_id.t -> unit -> unit;
    set_mempool :
      head:Tezos_crypto.Block_hash.t -> Mempool.t -> unit tzresult Lwt.t;
  }

  (** Documented in the ml file, because this is only exported for tests. *)
  type worker_tools = {
    push_request :
      (unit, Empty.t) Prevalidator_worker_state.Request.t -> bool Lwt.t;
    push_request_now :
      (unit, Empty.t) Prevalidator_worker_state.Request.t -> unit;
  }

  (** The corresponding internal type of the mempool (see {!Prevalidator.S}),
      that is independent from the protocol. *)
  type ('a, 'b) types_state_shell

  (** Create a pristine value of {!type_state_shell} *)
  val mk_types_state_shell :
    predecessor:Store.Block.t ->
    tools:'prevalidation_t tools ->
    worker:worker_tools ->
    ('protocol_data, 'prevalidation_t) types_state_shell

  module Make
      (Filter : Legacy_mempool_plugin.FILTER)
      (Prevalidation_t : Legacy_prevalidation.T
                           with type validation_state =
                             Filter.Proto.validation_state
                            and type protocol_operation = Filter.Proto.operation
                            and type operation_receipt =
                             Filter.Proto.operation_receipt) : sig
    (** The corresponding internal type of the mempool (see {!Prevalidator.S}),
        that depends on the protocol *)
    type types_state

    (** Create a pristine value of {!type_state} *)
    val mk_types_state :
      shell:
        ( Prevalidation_t.protocol_operation,
          Prevalidation_t.t )
        types_state_shell ->
      validation_state:Prevalidation_t.t ->
      types_state Lwt.t

    (** [to_shell pv] returns the shell part of [pv] *)
    val to_shell :
      types_state ->
      (Prevalidation_t.protocol_operation, Prevalidation_t.t) types_state_shell

    (** Documented in the ml file. *)
    val handle_unprocessed : types_state -> unit Lwt.t

    (** Documented in the ml file (as are all the functions of this module) *)
    module Requests : sig
      val on_advertise : _ types_state_shell -> unit

      val on_arrived :
        types_state ->
        Tezos_crypto.Operation_hash.t ->
        Operation.t ->
        (unit, Empty.t) result Lwt.t

      val on_ban :
        types_state -> Tezos_crypto.Operation_hash.t -> unit tzresult Lwt.t

      val on_flush :
        handle_branch_refused:bool ->
        types_state ->
        Store.Block.t ->
        Tezos_crypto.Block_hash.Set.t ->
        Tezos_crypto.Operation_hash.Set.t ->
        unit tzresult Lwt.t

      val on_inject :
        types_state -> force:bool -> Operation.t -> unit tzresult Lwt.t

      val on_notify :
        _ types_state_shell -> P2p_peer_id.t -> Mempool.t -> unit Lwt.t
    end
  end
end
