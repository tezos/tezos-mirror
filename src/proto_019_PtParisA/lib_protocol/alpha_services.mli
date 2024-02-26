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

open Alpha_context

module Seed_computation : sig
  val get :
    'a #RPC_context.simple ->
    'a ->
    Seed.seed_computation_status shell_tzresult Lwt.t
end

module Seed : sig
  val get : 'a #RPC_context.simple -> 'a -> Seed.seed shell_tzresult Lwt.t
end

module Nonce : sig
  type info = Revealed of Nonce.t | Missing of Nonce_hash.t | Forgotten

  val get :
    'a #RPC_context.simple -> 'a -> Raw_level.t -> info shell_tzresult Lwt.t
end

module Contract = Contract_services
module Constants = Constants_services
module Delegate = Delegate_services
module Voting = Voting_services
module Sapling = Sapling_services

module Liquidity_baking : sig
  val get_cpmm_address :
    'a #RPC_context.simple -> 'a -> Contract_hash.t shell_tzresult Lwt.t
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
end

module Denunciations : sig
  val denunciations :
    'a #RPC_context.simple ->
    'a ->
    (public_key_hash * Denunciations_repr.item) list shell_tzresult Lwt.t
end

val register : unit -> unit
