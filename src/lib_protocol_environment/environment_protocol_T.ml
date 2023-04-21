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
module type T = Environment_protocol_T_V10.T
(* Documentation for this interface may be found in
   module type [PROTOCOL] of [sigs/v6/updater.mli]. *)

module V0toV10
    (E : Environment_protocol_T_V0.T
           with type context := Context.t
            and type quota := quota
            and type validation_result := validation_result
            and type rpc_context := rpc_context
            and type tztrace := Error_monad.tztrace
            and type 'a tzresult := 'a Error_monad.tzresult) :
  Environment_protocol_T_V10.T
    with type context := Context.t
     and type quota := quota
     and type validation_result := validation_result
     and type rpc_context := rpc_context
     and type tztrace := Error_monad.tztrace
     and type 'a tzresult := 'a Error_monad.tzresult
     and type block_header_data = E.block_header_data
     and type block_header = E.block_header
     and type block_header_metadata = E.block_header_metadata
     and type operation_data = E.operation_data
     and type operation = E.operation
     and type operation_receipt = E.operation_receipt
     and type validation_state = E.validation_state
     and type application_state = E.validation_state
     and type cache_key = Context.Cache.key
     and type cache_value = Context.Cache.value = struct
  include E

  type application_state = validation_state

  type mode =
    | Application of block_header
    | Partial_validation of block_header
    | Construction of {
        predecessor_hash : Tezos_crypto.Hashed.Block_hash.t;
        timestamp : Time.Protocol.t;
        block_header_data : block_header_data;
      }
    | Partial_construction of {
        predecessor_hash : Tezos_crypto.Hashed.Block_hash.t;
        timestamp : Time.Protocol.t;
      }

  let begin_validation_or_application validation_or_application ctxt chain_id
      mode ~(predecessor : Block_header.shell_header) =
    match (validation_or_application, mode) with
    | `Validation, Application block_header | _, Partial_validation block_header
      ->
        (* For the validation of an existing block, we always use the
           old [begin_partial_application], even in full [Application]
           mode. Indeed, this maintains the behavior of old block
           [precheck] (from [lib_validation/block_validation.ml]), which
           relied on [Partial_validation] mode to quickly assess the
           viability of the block. *)
        begin_partial_application
          ~chain_id
          ~ancestor_context:ctxt
          ~predecessor_timestamp:predecessor.timestamp
          ~predecessor_fitness:predecessor.fitness
          block_header
    | `Application, Application block_header ->
        begin_application
          ~chain_id
          ~predecessor_context:ctxt
          ~predecessor_timestamp:predecessor.timestamp
          ~predecessor_fitness:predecessor.fitness
          block_header
    | _, Construction {predecessor_hash; timestamp; block_header_data} ->
        begin_construction
          ~chain_id
          ~predecessor_context:ctxt
          ~predecessor_timestamp:predecessor.timestamp
          ~predecessor_level:predecessor.level
          ~predecessor_fitness:predecessor.fitness
          ~predecessor:predecessor_hash
          ~timestamp
          ~protocol_data:block_header_data
          ()
    | _, Partial_construction {predecessor_hash; timestamp} ->
        begin_construction
          ~chain_id
          ~predecessor_context:ctxt
          ~predecessor_timestamp:predecessor.timestamp
          ~predecessor_level:predecessor.level
          ~predecessor_fitness:predecessor.fitness
          ~predecessor:predecessor_hash
          ~timestamp
          ()

  let begin_validation = begin_validation_or_application `Validation

  let begin_application = begin_validation_or_application `Application

  let validate_operation ?check_signature:_ validation_state _oph operation =
    let open Lwt_result_syntax in
    let* validation_state, _operation_receipt =
      apply_operation validation_state operation
    in
    return validation_state

  let apply_operation application_state _oph operation =
    apply_operation application_state operation

  let finalize_validation validation_state =
    let open Lwt_result_syntax in
    let* _ = finalize_block validation_state in
    return_unit

  let finalize_application application_state _shell_header =
    finalize_block application_state

  let compare_operations (_, op) (_, op') = compare_operations op op'

  let acceptable_pass op =
    match acceptable_passes op with [n] -> Some n | _ -> None

  let value_of_key ~chain_id:_ ~predecessor_context:_ ~predecessor_timestamp:_
      ~predecessor_level:_ ~predecessor_fitness:_ ~predecessor:_ ~timestamp:_ =
    Lwt.return_ok (fun _ ->
        Lwt.return
          (Error_monad.error_with
             "element_of_key called on environment protocol < V3"))

  type cache_key = Context.Cache.key

  type cache_value = Context.Cache.value

  let init _chain_id c hd = init c hd

  (* Fake mempool that can be successfully initialized but cannot
     accept any operations. *)
  module Mempool = struct
    type t = unit

    type validation_info = unit

    type conflict_handler =
      existing_operation:Tezos_crypto.Hashed.Operation_hash.t * operation ->
      new_operation:Tezos_crypto.Hashed.Operation_hash.t * operation ->
      [`Keep | `Replace]

    type operation_conflict =
      | Operation_conflict of {
          existing : Tezos_crypto.Hashed.Operation_hash.t;
          new_operation : Tezos_crypto.Hashed.Operation_hash.t;
        }

    type add_result =
      | Added
      | Replaced of {removed : Tezos_crypto.Hashed.Operation_hash.t}
      | Unchanged

    type add_error =
      | Validation_error of error trace
      | Add_conflict of operation_conflict

    type merge_error =
      | Incompatible_mempool
      | Merge_conflict of operation_conflict

    let init _ _ ~head_hash:_ ~head:_ = Lwt.return_ok ((), ())

    let encoding = Data_encoding.unit

    let add_operation ?check_signature:_ ?conflict_handler:_ _ _ _ =
      let msg =
        "The mempool cannot accept any operations because it does not support \
         the current protocol."
      in
      Lwt.return_error (Validation_error [Exn (Failure msg)])

    let remove_operation () _ = ()

    let merge ?conflict_handler:_ () () = Ok ()

    let operations () = Tezos_crypto.Hashed.Operation_hash.Map.empty
  end
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
       and type tztrace := Error_monad.tztrace
       and type 'a tzresult := 'a Error_monad.tzresult
       and type cache_key := Context.Cache.key
       and type cache_value := Context.Cache.value

  val set_log_message_consumer :
    (Internal_event.level -> string -> unit) -> unit

  val environment_version : Protocol.env_version

  val expected_context_hash : header_context_hash_semantics

  val begin_validation :
    Context.t ->
    Tezos_crypto.Hashed.Chain_id.t ->
    mode ->
    predecessor:Block_header.shell_header ->
    cache:Context.source_of_cache ->
    validation_state Error_monad.tzresult Lwt.t

  val begin_application :
    Context.t ->
    Tezos_crypto.Hashed.Chain_id.t ->
    mode ->
    predecessor:Block_header.shell_header ->
    cache:Context.source_of_cache ->
    application_state Error_monad.tzresult Lwt.t

  module Mempool : sig
    include module type of Mempool

    val init :
      Context.t ->
      Tezos_crypto.Hashed.Chain_id.t ->
      head_hash:Tezos_crypto.Hashed.Block_hash.t ->
      head:Block_header.shell_header ->
      cache:Context.source_of_cache ->
      (validation_info * t) tzresult Lwt.t
  end
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
            and type tztrace := Error_monad.tztrace
            and type 'a tzresult := 'a Error_monad.tzresult) =
struct
  include P

  let init chain_id context header =
    let open Lwt_syntax in
    let* context = Context.Cache.set_cache_layout context [] in
    init chain_id context header

  let begin_validation ctxt chain_id mode ~predecessor ~cache:_ =
    begin_validation ctxt chain_id mode ~predecessor

  let begin_application ctxt chain_id mode ~predecessor ~cache:_ =
    begin_application ctxt chain_id mode ~predecessor

  module Mempool = struct
    include Mempool

    let init ctxt chain_id ~head_hash ~head ~cache:_ =
      init ctxt chain_id ~head_hash ~head
  end
end
