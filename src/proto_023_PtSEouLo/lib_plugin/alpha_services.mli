(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019-2020 Nomadic Labs <contact@nomadic-labs.com>           *)
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

(** This declares Protocol RPC services.

    Protocol RPC services are read-only, and support querying the state of the
    ledger (including information such as existing contracts, delegation,
    voting, and so on), at a given block height.

    This is a mostly internal module used from [rpc_services] in [Main].
 *)

open Protocol
open Environment
open Error_monad
open Alpha_context

module Seed_computation : sig
  val get :
    'a #RPC_context.simple ->
    'a ->
    Seed.seed_computation_status shell_tzresult Lwt.t

  module S : sig
    val seed_computation_status_encoding :
      Seed.seed_computation_status Data_encoding.t

    val seed_computation :
      ( [`GET],
        Updater.rpc_context,
        Updater.rpc_context,
        unit,
        unit,
        Seed.seed_computation_status )
      RPC_service.t
  end
end

module Seed : sig
  val get : 'a #RPC_context.simple -> 'a -> Seed.seed shell_tzresult Lwt.t

  module S : sig
    val seed :
      ( [`POST],
        Updater.rpc_context,
        Updater.rpc_context,
        unit,
        unit,
        Seed.seed )
      RPC_service.t
  end
end

module Nonce : sig
  type info = Revealed of Nonce.t | Missing of Nonce_hash.t | Forgotten

  val get :
    'a #RPC_context.simple -> 'a -> Raw_level.t -> info shell_tzresult Lwt.t

  module S : sig
    val get :
      ( [`GET],
        Updater.rpc_context,
        Updater.rpc_context * Raw_level.t,
        unit,
        unit,
        info )
      RPC_service.t
  end
end

module Constants = Constants_services
module Voting = Voting_services
module Sapling = Sapling_services

module Liquidity_baking : sig
  val get_cpmm_address :
    'a #RPC_context.simple -> 'a -> Contract_hash.t shell_tzresult Lwt.t

  module S : sig
    val get_cpmm_address :
      ( [`GET],
        Updater.rpc_context,
        Updater.rpc_context,
        unit,
        unit,
        Contract_hash.t )
      RPC_service.t
  end
end

module Cache : sig
  val cached_contracts :
    'a #RPC_context.simple ->
    'a ->
    (Contract_hash.t * int) list shell_tzresult Lwt.t

  val contract_cache_size :
    'a #RPC_context.simple -> 'a -> int shell_tzresult Lwt.t

  val contract_cache_size_limit :
    'a #RPC_context.simple -> 'a -> int shell_tzresult Lwt.t

  val contract_rank :
    'a #RPC_context.simple ->
    'a ->
    Contract_hash.t ->
    int option shell_tzresult Lwt.t

  module S : sig
    val cached_contracts :
      ( [`GET],
        Updater.rpc_context,
        Updater.rpc_context,
        unit,
        unit,
        (Contract_hash.t * int) list )
      RPC_service.t

    val contract_cache_size :
      ( [`GET],
        Updater.rpc_context,
        Updater.rpc_context,
        unit,
        unit,
        int )
      RPC_service.t

    val contract_cache_size_limit :
      ( [`GET],
        Updater.rpc_context,
        Updater.rpc_context,
        unit,
        unit,
        int )
      RPC_service.t

    val contract_rank :
      ( [`POST],
        Updater.rpc_context,
        Updater.rpc_context,
        unit,
        Contract_hash.t,
        int option )
      RPC_service.t
  end
end

module Denunciations : sig
  type denunciations_with_key = public_key_hash * Denunciations_repr.item

  val denunciations_with_key_encoding : denunciations_with_key Data_encoding.t

  val denunciations :
    'a #RPC_context.simple ->
    'a ->
    denunciations_with_key list shell_tzresult Lwt.t

  module S : sig
    val denunciations :
      ( [`GET],
        Updater.rpc_context,
        Updater.rpc_context,
        unit,
        unit,
        denunciations_with_key list )
      RPC_service.t
  end
end

val register : unit -> unit
