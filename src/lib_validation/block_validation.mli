(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018 Nomadic Labs. <nomadic@tezcore.com>                    *)
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

type validation_store = {
  context_hash : Context_hash.t;
  message : string option;
  max_operations_ttl : int;
  last_allowed_fork_level : Int32.t;
}

val may_patch_protocol :
  user_activated_upgrades:User_activated.upgrades ->
  user_activated_protocol_overrides:User_activated.protocol_overrides ->
  level:Int32.t ->
  Tezos_protocol_environment.validation_result ->
  Tezos_protocol_environment.validation_result Lwt.t

val update_testchain_status :
  Context.t -> Block_header.t -> Time.Protocol.t -> Context.t tzresult Lwt.t

(** [init_test_chain] must only be called on a forking block. *)
val init_test_chain :
  Context.t -> Block_header.t -> Block_header.t tzresult Lwt.t

val check_liveness :
  live_blocks:Block_hash.Set.t ->
  live_operations:Operation_hash.Set.t ->
  Block_hash.t ->
  Operation.t list list ->
  unit tzresult Lwt.t

type result = {
  validation_store : validation_store;
  block_metadata : Bytes.t;
  ops_metadata : Bytes.t list list;
  forking_testchain : bool;
}

val result_encoding : result Data_encoding.t

val apply :
  Chain_id.t ->
  user_activated_upgrades:User_activated.upgrades ->
  user_activated_protocol_overrides:User_activated.protocol_overrides ->
  max_operations_ttl:int ->
  predecessor_block_header:Block_header.t ->
  predecessor_context:Context.t ->
  block_header:Block_header.t ->
  Operation.t list list ->
  result tzresult Lwt.t
