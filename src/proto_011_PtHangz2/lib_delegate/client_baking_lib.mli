(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(** Mine a block *)
val bake_block :
  #Protocol_client_context.full ->
  ?minimal_fees:Tez.t ->
  ?minimal_nanotez_per_gas_unit:Q.t ->
  ?minimal_nanotez_per_byte:Q.t ->
  ?force:bool ->
  ?max_priority:int ->
  ?minimal_timestamp:bool ->
  ?ignore_node_mempool:bool ->
  ?extra_operations:Client_baking_forge.Operations_source.t ->
  ?context_path:string ->
  ?src_sk:Client_keys.sk_uri ->
  liquidity_baking_escape_vote:bool ->
  chain:Chain_services.chain ->
  head:Block_services.block ->
  public_key_hash ->
  unit tzresult Lwt.t

(** Endorse a block *)
val endorse_block :
  #Protocol_client_context.full ->
  chain:Chain_services.chain ->
  Client_keys.Public_key_hash.t ->
  unit Error_monad.tzresult Lwt.t

(** Get the previous cycle of the given cycle *)
val get_predecessor_cycle :
  #Protocol_client_context.full -> Cycle.t -> Cycle.t Lwt.t

(** Reveal the nonces used to bake each block in the given list *)
val reveal_block_nonces :
  #Protocol_client_context.full ->
  chain:Chain_services.chain ->
  block:Block_services.block ->
  Block_hash.t list ->
  unit Error_monad.tzresult Lwt.t

(** Reveal all unrevealed nonces *)
val reveal_nonces :
  #Protocol_client_context.full ->
  chain:Chain_services.chain ->
  block:Block_services.block ->
  unit ->
  unit Error_monad.tzresult Lwt.t
