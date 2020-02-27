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

type validator_kind =
  | Internal of Context.index
  | External of {
      context_root : string;
      protocol_root : string;
      process_path : string;
      sandbox_parameters : Data_encoding.json option;
    }

type t

val init :
  genesis:Genesis.t ->
  user_activated_upgrades:User_activated.upgrades ->
  user_activated_protocol_overrides:User_activated.protocol_overrides ->
  validator_kind ->
  t tzresult Lwt.t

val close : t -> unit Lwt.t

val restore_context_integrity : t -> int option tzresult Lwt.t

val apply_block :
  t ->
  predecessor:State.Block.t ->
  Block_header.t ->
  Operation.t list list ->
  Block_validation.result tzresult Lwt.t

val commit_genesis : t -> chain_id:Chain_id.t -> Context_hash.t tzresult Lwt.t

(** [init_test_chain] must only be called on a forking block. *)
val init_test_chain : t -> State.Block.t -> Block_header.t tzresult Lwt.t
