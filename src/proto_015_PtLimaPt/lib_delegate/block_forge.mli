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

type unsigned_block = {
  unsigned_block_header : Block_header.t;
  operations : Tezos_base.Operation.t list list;
}

type simulation_kind =
  | Filter of Operation_pool.Prioritized.t
  | Apply of {
      ordered_pool : Operation_pool.ordered_pool;
      payload_hash : Block_payload_hash.t;
    }

type simulation_mode = Local of Context.index | Node

val forge_faked_protocol_data :
  ?payload_hash:Block_payload_hash.t ->
  payload_round:Round.t ->
  seed_nonce_hash:Nonce_hash.t option ->
  liquidity_baking_toggle_vote:Liquidity_baking.liquidity_baking_toggle_vote ->
  unit ->
  block_header_data

val forge :
  #Protocol_client_context.full ->
  chain_id:Chain_id.t ->
  pred_info:Baking_state.block_info ->
  timestamp:Time.Protocol.t ->
  liquidity_baking_toggle_vote:Liquidity_baking.liquidity_baking_toggle_vote ->
  user_activated_upgrades:User_activated.upgrades ->
  Baking_configuration.fees_config ->
  seed_nonce_hash:Nonce_hash.t option ->
  payload_round:Round.t ->
  Baking_state.validation_mode ->
  simulation_kind ->
  Constants.Parametric.t ->
  unsigned_block tzresult Lwt.t
