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

open Alpha_context

val list : 'a #RPC_context.simple -> 'a -> Contract.t list shell_tzresult Lwt.t

type info = {
  balance : Tez.t;
  delegate : public_key_hash option;
  counter : Manager_counter.t option;
  script : Script.t option;
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
  Unstake_requests.prepared_finalize_unstake option shell_tzresult Lwt.t

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
  Script.t shell_tzresult Lwt.t

val script_opt :
  'a #RPC_context.simple ->
  'a ->
  Contract_hash.t ->
  Script.t option shell_tzresult Lwt.t

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

val register : unit -> unit
