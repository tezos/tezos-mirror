(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

open Environment_context

(* This module contains the real module signature of an economic
   protocol that the Shell sees. There is actually only one signature
   to avoid [if]-[then]-[else] expressions inside the Shell.

   When we change the module signature output of the environment, we
   need to implement a forward-compatible interface. This is done by
   upgrading the old interface to the new one.

   The first change in this signature was introduced by the [V3]
   environment. This is why we implement a functor from the initial
   environment [V0] to [V3] directly because neither [V1] nor [V2]
   change the module signature output of the environment.

   All the equalities constraints are here for typing only. We use a
   destructive substitution ([:=]) for types that are defined by the
   shell, or that are common to all the economic protocol
   environments, and an equality constraint ([=]) for the types that
   are abstracted from the economic protocol.

   [module type T] defines the same signature as the last [Vx]
   environment ([module type Vx_T]).

   If you want to mock this module type, see {!Environment_protocol_T_test}. *)
module type T = sig
  (* Documentation for this interface may be found in
     module type [PROTOCOL] of [sigs/v3/updater.mli]. *)

  include Environment_protocol_T_V3.T

  val set_log_message_consumer :
    (Internal_event.level -> string -> unit) -> unit

  val environment_version : Protocol.env_version
end

module V0toV3
    (E : Environment_protocol_T_V0.T
           with type context := Context.t
            and type quota := quota
            and type validation_result := validation_result
            and type rpc_context := rpc_context
            and type 'a tzresult := 'a Error_monad.tzresult) :
  Environment_protocol_T_V3.T
    with type context := Context.t
     and type quota := quota
     and type validation_result := validation_result
     and type rpc_context := rpc_context
     and type 'a tzresult := 'a Error_monad.tzresult
     and type block_header_data = E.block_header_data
     and type block_header = E.block_header
     and type block_header_metadata = E.block_header_metadata
     and type operation_data = E.operation_data
     and type operation = E.operation
     and type operation_receipt = E.operation_receipt
     and type validation_state = E.validation_state
     and type cache_key = Context.Cache.key
     and type cache_value = Context.Cache.value = struct
  include E

  let finalize_block vs _ = E.finalize_block vs

  (* Add backwards compatibility shadowing here *)
  let relative_position_within_block = compare_operations

  let value_of_key ~chain_id:_ ~predecessor_context:_ ~predecessor_timestamp:_
      ~predecessor_level:_ ~predecessor_fitness:_ ~predecessor:_ ~timestamp:_ =
    Lwt.return_ok (fun _ ->
        Lwt.return
          (Error_monad.error_with
             "element_of_key called on environment protocol < V3"))

  type cache_key = Context.Cache.key

  type cache_value = Context.Cache.value
end

(* [module type PROTOCOL] is protocol signature that the shell can use.

   A module of this signature is typically obtained through an adapter
   (see Lift functors in environment definitions) of the Main module
   (which complies with the [Updater] signature).
*)
module type PROTOCOL = sig
  include
    T
      with type context := Context.t
       and type quota := quota
       and type validation_result := validation_result
       and type rpc_context := rpc_context
       and type 'a tzresult := 'a Error_monad.tzresult
       and type cache_key := Context.Cache.key
       and type cache_value := Context.Cache.value

  val environment_version : Protocol.env_version

  val begin_partial_application :
    chain_id:Chain_id.t ->
    ancestor_context:Context.t ->
    predecessor:Block_header.t ->
    predecessor_hash:Block_hash.t ->
    cache:Context.source_of_cache ->
    block_header ->
    (validation_state, tztrace) result Lwt.t

  val begin_application :
    chain_id:Chain_id.t ->
    predecessor_context:Context.t ->
    predecessor_timestamp:Time.Protocol.t ->
    predecessor_fitness:Fitness.t ->
    cache:Context.source_of_cache ->
    block_header ->
    validation_state Error_monad.tzresult Lwt.t

  val begin_construction :
    chain_id:Chain_id.t ->
    predecessor_context:Context.t ->
    predecessor_timestamp:Time.Protocol.t ->
    predecessor_level:Int32.t ->
    predecessor_fitness:Fitness.t ->
    predecessor:Block_hash.t ->
    timestamp:Time.Protocol.t ->
    ?protocol_data:block_header_data ->
    cache:Context.source_of_cache ->
    unit ->
    validation_state Error_monad.tzresult Lwt.t

  val finalize_block :
    validation_state ->
    Block_header.shell_header option ->
    (validation_result * block_header_metadata) tzresult Lwt.t
end

(*

   For environment V where V < V3, the caching mechanism is ignored.
   The following functor provides a protocol adapter to implement
   this.

*)
module IgnoreCaches
    (P : T
           with type context := Context.t
            and type quota := quota
            and type validation_result := validation_result
            and type rpc_context := rpc_context
            and type 'a tzresult := 'a Error_monad.tzresult) =
struct
  include P

  let init context header =
    let open Lwt_syntax in
    let* context = Context.Cache.set_cache_layout context [] in
    init context header

  let begin_partial_application ~chain_id ~ancestor_context
      ~(predecessor : Block_header.t) ~predecessor_hash:_ ~cache:_
      (raw_block : block_header) =
    begin_partial_application
      ~chain_id
      ~ancestor_context
      ~predecessor_timestamp:predecessor.shell.timestamp
      ~predecessor_fitness:predecessor.shell.fitness
      raw_block

  let begin_application ~chain_id ~predecessor_context ~predecessor_timestamp
      ~predecessor_fitness ~cache:_ raw_block =
    begin_application
      ~chain_id
      ~predecessor_context
      ~predecessor_timestamp
      ~predecessor_fitness
      raw_block

  let begin_construction ~chain_id ~predecessor_context ~predecessor_timestamp
      ~predecessor_level ~predecessor_fitness ~predecessor ~timestamp
      ?protocol_data ~cache:_ () =
    begin_construction
      ~chain_id
      ~predecessor_context
      ~predecessor_timestamp
      ~predecessor_level
      ~predecessor_fitness
      ~predecessor
      ~timestamp
      ?protocol_data
      ()

  let finalize_block c shell_header = P.finalize_block c shell_header
end
