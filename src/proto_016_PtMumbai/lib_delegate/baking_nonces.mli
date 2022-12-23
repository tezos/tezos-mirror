(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

type state = {
  cctxt : Protocol_client_context.full;
  chain : Chain_services.chain;
  constants : Constants.t;
  config : Baking_configuration.nonce_config;
  nonces_location : [`Nonce] Baking_files.location;
  mutable last_predecessor : Block_hash.t;
}

type t = state

type nonces = Nonce.t Block_hash.Map.t

val empty : Nonce.t Block_hash.Map.t

val encoding : Nonce.t Block_hash.Map.t Data_encoding.t

val load :
  #Client_context.wallet ->
  [< `Highwatermarks | `Nonce | `State] Baking_files.location ->
  Nonce.t Block_hash.Map.t tzresult Lwt.t

val save :
  #Client_context.wallet ->
  [< `Highwatermarks | `Nonce | `State] Baking_files.location ->
  Nonce.t Block_hash.Map.t ->
  unit tzresult Lwt.t

val mem : Nonce.t Block_hash.Map.t -> Block_hash.t -> bool

val find_opt : Nonce.t Block_hash.Map.t -> Block_hash.t -> Nonce.t option

val get_block_level_opt :
  #Tezos_rpc.Context.simple ->
  chain:Block_services.chain ->
  block:Block_services.block ->
  int32 option Lwt.t

val get_outdated_nonces :
  t ->
  Nonce.t Block_hash.Map.t ->
  (Nonce.t Block_hash.Map.t * Nonce.t Block_hash.Map.t) tzresult Lwt.t

val filter_outdated_nonces :
  t -> Nonce.t Block_hash.Map.t -> Nonce.t Block_hash.Map.t tzresult Lwt.t

val blocks_from_current_cycle :
  t ->
  Block_services.block ->
  ?offset:int32 ->
  unit ->
  Block_hash.t list tzresult Lwt.t

val get_unrevealed_nonces :
  t -> Nonce.t Block_hash.Map.t -> (Raw_level.t * Nonce.t) list tzresult Lwt.t

val generate_seed_nonce :
  Baking_configuration.nonce_config ->
  Baking_state.consensus_key ->
  Raw_level.t ->
  (Nonce_hash.t * Nonce.t) tzresult Lwt.t

val register_nonce :
  #Protocol_client_context.full ->
  chain_id:Chain_id.t ->
  Block_hash.t ->
  Nonce.t ->
  unit tzresult Lwt.t

val inject_seed_nonce_revelation :
  #Protocol_client_context.full ->
  chain:Chain_services.chain ->
  block:Block_services.block ->
  branch:Block_hash.t ->
  (Raw_level.t * Nonce.t) list ->
  unit tzresult Lwt.t

val reveal_potential_nonces : t -> Baking_state.proposal -> unit tzresult Lwt.t

val start_revelation_worker :
  Protocol_client_context.full ->
  Baking_configuration.nonce_config ->
  Chain_id.t ->
  Constants.t ->
  Baking_state.proposal Lwt_stream.t ->
  Lwt_canceler.t Lwt.t
