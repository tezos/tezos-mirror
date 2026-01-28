(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019-2022 Nomadic Labs <contact@nomadic-labs.com>           *)
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

(** This module defines RPC services to access the information associated to
    contracts (balance, delegate, script, etc.).
*)

open Environment
open Environment.Error_monad
open Protocol
open Protocol.Alpha_context

val list : 'a #RPC_context.simple -> 'a -> Contract.t list shell_tzresult Lwt.t

type info = {
  balance : Tez.t;
  delegate : public_key_hash option;
  counter : Manager_counter.t option;
  script : Script.t option;
  revealed : bool option;
}

val info_encoding : info Data_encoding.t

val info :
  'a #RPC_context.simple ->
  'a ->
  Contract.t ->
  normalize_types:bool ->
  info shell_tzresult Lwt.t

val balance :
  'a #RPC_context.simple -> 'a -> Contract.t -> Tez.t shell_tzresult Lwt.t

val frozen_bonds :
  'a #RPC_context.simple -> 'a -> Contract.t -> Tez.t shell_tzresult Lwt.t

val balance_and_frozen_bonds :
  'a #RPC_context.simple -> 'a -> Contract.t -> Tez.t shell_tzresult Lwt.t

val staked_balance :
  'a #RPC_context.simple ->
  'a ->
  Contract.t ->
  Tez.t option shell_tzresult Lwt.t

val staking_numerator :
  'a #RPC_context.simple ->
  'a ->
  Contract.t ->
  Staking_pseudotoken.t shell_tzresult Lwt.t

val unstaked_frozen_balance :
  'a #RPC_context.simple ->
  'a ->
  Contract.t ->
  Tez.t option shell_tzresult Lwt.t

val unstaked_finalizable_balance :
  'a #RPC_context.simple ->
  'a ->
  Contract.t ->
  Tez.t option shell_tzresult Lwt.t

val unstake_requests :
  'a #RPC_context.simple ->
  'a ->
  Contract.t ->
  Unstake_requests.For_RPC.prepared_finalize_unstake option shell_tzresult Lwt.t

val full_balance :
  'a #RPC_context.simple -> 'a -> Contract.t -> Tez.t shell_tzresult Lwt.t

val manager_key :
  'a #RPC_context.simple ->
  'a ->
  public_key_hash ->
  public_key option shell_tzresult Lwt.t

val delegate :
  'a #RPC_context.simple ->
  'a ->
  Contract.t ->
  public_key_hash shell_tzresult Lwt.t

val delegate_opt :
  'a #RPC_context.simple ->
  'a ->
  Contract.t ->
  public_key_hash option shell_tzresult Lwt.t

val counter :
  'a #RPC_context.simple ->
  'a ->
  public_key_hash ->
  Manager_counter.t shell_tzresult Lwt.t

val script :
  'a #RPC_context.simple ->
  'a ->
  Contract_hash.t ->
  Script.michelson_with_storage shell_tzresult Lwt.t

val script_opt :
  'a #RPC_context.simple ->
  'a ->
  Contract_hash.t ->
  Script.michelson_with_storage option shell_tzresult Lwt.t

val storage :
  'a #RPC_context.simple ->
  'a ->
  Contract_hash.t ->
  Script.expr shell_tzresult Lwt.t

val entrypoint_type :
  'a #RPC_context.simple ->
  'a ->
  Contract_hash.t ->
  Entrypoint.t ->
  normalize_types:bool ->
  Script.expr shell_tzresult Lwt.t

val list_entrypoints :
  'a #RPC_context.simple ->
  'a ->
  Contract_hash.t ->
  normalize_types:bool ->
  (Michelson_v1_primitives.prim list list * (string * Script.expr) list)
  shell_tzresult
  Lwt.t

val storage_opt :
  'a #RPC_context.simple ->
  'a ->
  Contract_hash.t ->
  Script.expr option shell_tzresult Lwt.t

val estimated_own_pending_slashed_amount :
  'a #RPC_context.simple ->
  'a ->
  Signature.public_key_hash ->
  Tez.t shell_tzresult Lwt.t

val big_map_get :
  'a #RPC_context.simple ->
  'a ->
  Big_map.Id.t ->
  Script_expr_hash.t ->
  Script.expr shell_tzresult Lwt.t

val contract_big_map_get_opt :
  'a #RPC_context.simple ->
  'a ->
  Contract_hash.t ->
  Script.expr * Script.expr ->
  Script.expr option shell_tzresult Lwt.t

val single_sapling_get_diff :
  'a #RPC_context.simple ->
  'a ->
  Contract_hash.t ->
  ?offset_commitment:int64 ->
  ?offset_nullifier:int64 ->
  unit ->
  (Sapling.root * Sapling.diff) shell_tzresult Lwt.t

val clst_contract_hash :
  'a #RPC_context.simple -> 'a -> Contract_hash.t shell_tzresult Lwt.t

val clst_balance :
  'a #RPC_context.simple ->
  'a ->
  Contract.t ->
  Script_int.n Script_int.num shell_tzresult Lwt.t

val clst_total_supply :
  'a #RPC_context.simple ->
  'a ->
  Script_int.n Script_int.num shell_tzresult Lwt.t

val clst_total_amount_of_tez :
  'a #RPC_context.simple -> 'a -> Tez.t shell_tzresult Lwt.t

val clst_exchange_rate :
  'a #RPC_context.simple -> 'a -> Q.t shell_tzresult Lwt.t

val register : unit -> unit

(** Functions used in the implementation of this file's RPCs, but also
    useful elsewhere (as opposed to the functions above, which call
    the RPCs). These functions are gathered in a separate module to
    avoid naming conflicts. *)
module Implem : sig
  val unstake_requests :
    Alpha_context.t ->
    Contract.t ->
    Unstake_requests.For_RPC.prepared_finalize_unstake option
    Environment.Error_monad.tzresult
    Lwt.t
end

module S : sig
  val balance :
    ( [`GET],
      RPC_context.t,
      RPC_context.t * Contract.t,
      unit,
      unit,
      Tez.t )
    RPC_service.service

  val spendable :
    ( [`GET],
      RPC_context.t,
      RPC_context.t * Contract.t,
      unit,
      unit,
      Tez.t )
    RPC_service.service

  val frozen_bonds :
    ( [`GET],
      RPC_context.t,
      RPC_context.t * Contract.t,
      unit,
      unit,
      Tez.t )
    RPC_service.service

  val balance_and_frozen_bonds :
    ( [`GET],
      RPC_context.t,
      RPC_context.t * Contract.t,
      unit,
      unit,
      Tez.t )
    RPC_service.service

  val spendable_and_frozen_bonds :
    ( [`GET],
      RPC_context.t,
      RPC_context.t * Contract.t,
      unit,
      unit,
      Tez.t )
    RPC_service.service

  val staked_balance :
    ( [`GET],
      RPC_context.t,
      RPC_context.t * Contract.t,
      unit,
      unit,
      Tez.t option )
    RPC_service.service

  val staking_numerator :
    ( [`GET],
      RPC_context.t,
      RPC_context.t * Contract.t,
      unit,
      unit,
      Staking_pseudotoken.t )
    RPC_service.service

  val unstaked_frozen_balance :
    ( [`GET],
      RPC_context.t,
      RPC_context.t * Contract.t,
      unit,
      unit,
      Tez.t option )
    RPC_service.service

  val unstaked_finalizable_balance :
    ( [`GET],
      RPC_context.t,
      RPC_context.t * Contract.t,
      unit,
      unit,
      Tez.t option )
    RPC_service.service

  val unstake_requests :
    ( [`GET],
      RPC_context.t,
      RPC_context.t * Contract.t,
      unit,
      unit,
      Unstake_requests.For_RPC.prepared_finalize_unstake option )
    RPC_service.service

  val full_balance :
    ( [`GET],
      RPC_context.t,
      RPC_context.t * Contract.t,
      unit,
      unit,
      Tez.t )
    RPC_service.service

  val manager_key :
    ( [`GET],
      RPC_context.t,
      RPC_context.t * Contract.t,
      unit,
      unit,
      Signature.Public_key.t option )
    RPC_service.service

  val delegate :
    ( [`GET],
      RPC_context.t,
      RPC_context.t * Contract.t,
      unit,
      unit,
      Signature.Public_key_hash.t )
    RPC_service.service

  val counter :
    ( [`GET],
      RPC_context.t,
      RPC_context.t * Contract.t,
      unit,
      unit,
      Manager_counter.t )
    RPC_service.service

  val script :
    ( [`GET],
      RPC_context.t,
      RPC_context.t * Contract.t,
      unit,
      unit,
      Script.michelson_with_storage )
    RPC_service.service

  val storage :
    ( [`GET],
      RPC_context.t,
      RPC_context.t * Contract.t,
      unit,
      unit,
      Script.expr )
    RPC_service.service

  type normalize_types_query = {normalize_types : bool}

  val normalize_types_query : normalize_types_query RPC_query.t

  val entrypoint_type :
    ( [`GET],
      RPC_context.t,
      (RPC_context.t * Contract.t) * Entrypoint.t,
      normalize_types_query,
      unit,
      Script.expr )
    RPC_service.service

  val list_entrypoints :
    ( [`GET],
      RPC_context.t,
      RPC_context.t * Contract.t,
      normalize_types_query,
      unit,
      Michelson_v1_primitives.prim list list * (string * Script.expr) list )
    RPC_service.service

  val contract_big_map_get_opt :
    ( [`POST],
      RPC_context.t,
      RPC_context.t * Contract.t,
      unit,
      Script.expr * Script.expr,
      Script.expr option )
    RPC_service.service

  val big_map_get :
    ( [`GET],
      RPC_context.t,
      (RPC_context.t * Big_map.Id.t) * Script_expr_hash.t,
      unit,
      unit,
      Script.expr )
    RPC_service.service

  type big_map_get_all_query = {offset : int option; length : int option}

  val rpc_arg_uint : int RPC_arg.t

  val big_map_get_all_query : big_map_get_all_query RPC_query.t

  val big_map_get_all :
    ( [`GET],
      RPC_context.t,
      RPC_context.t * Big_map.Id.t,
      big_map_get_all_query,
      unit,
      Script.expr list )
    RPC_service.service

  val info :
    ( [`GET],
      RPC_context.t,
      RPC_context.t * Contract.t,
      normalize_types_query,
      unit,
      info )
    RPC_service.service

  val list :
    ( [`GET],
      RPC_context.t,
      RPC_context.t,
      unit,
      unit,
      Contract.t list )
    RPC_service.service

  val estimated_own_pending_slashed_amount :
    ( [`GET],
      RPC_context.t,
      RPC_context.t * Contract.t,
      unit,
      unit,
      Tez.t )
    RPC_service.service

  module Sapling : sig
    val single_sapling_get_id :
      context ->
      Contract_hash.t ->
      ( Sapling.Id.t option * context,
        Error_monad.error Error_monad.trace )
      Pervasives.result
      Lwt.t

    val make_service :
      ('a, 'b) Sapling_services.S.Args.t ->
      ( [`GET],
        RPC_context.t,
        RPC_context.t * Contract.t,
        'a,
        unit,
        'b )
      RPC_service.service
      * (context ->
        Contract.t ->
        'a ->
        unit ->
        ('b option, Error_monad.error Error_monad.trace) Pervasives.result Lwt.t)

    val get_diff :
      ( [`GET],
        RPC_context.t,
        RPC_context.t * Contract.t,
        Sapling_services.diff_query,
        unit,
        Sapling.root * Sapling.diff )
      RPC_service.service
      * (context ->
        Contract.t ->
        Sapling_services.diff_query ->
        unit ->
        ( (Sapling.root * Sapling.diff) option,
          Error_monad.error Error_monad.trace )
        Pervasives.result
        Lwt.t)

    val register : unit -> unit

    val mk_call1 :
      ( [< Resto.meth],
        RPC_context.t,
        RPC_context.t * 'a,
        'b,
        unit,
        'c )
      RPC_service.t
      * 'd ->
      'e #RPC_context.simple ->
      'e ->
      'a ->
      'b ->
      'c Error_monad.shell_tzresult Lwt.t
  end
end
