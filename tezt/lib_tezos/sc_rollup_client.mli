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

(** Smart-Contract Rollup client state *)
type t

type commitment = {
  compressed_state : string;
  inbox_level : int;
  predecessor : string;
  number_of_messages : int;
  number_of_ticks : int;
}

(** [create ?name ?path ?base_dir ?path node] returns a fresh client
   identified by a specified [name], logging in [color], executing the
   program at [path], storing local information in [base_dir], and
   communicating with the specified [node]. *)
val create :
  ?name:string ->
  ?path:string ->
  ?base_dir:string ->
  ?color:Log.Color.t ->
  Sc_rollup_node.t ->
  t

(** [sc_rollup_address client] returns the smart contract rollup
   address of the node associated to the [client]. *)
val sc_rollup_address : t -> string Lwt.t

(** [rpc_get client path] issues a GET request for [path]. *)
val rpc_get : ?hooks:Process.hooks -> t -> Client.path -> JSON.t Lwt.t

(** [total_ticks client] gets the total number of ticks for the PVM. *)
val total_ticks : ?hooks:Process.hooks -> t -> int Lwt.t

(** [ticks client] gets the number of ticks for the PVM for the current head. *)
val ticks : ?hooks:Process.hooks -> t -> int Lwt.t

(** [state_hash client] gets the corresponding PVM state hash for the current head block. *)
val state_hash : ?hooks:Process.hooks -> t -> string Lwt.t

(** [status client] gets the corresponding PVM status for the current head block. *)
val status : ?hooks:Process.hooks -> t -> string Lwt.t

(** [last_stored_commitment client] gets the last commitment stored by the rollup node. *)
val last_stored_commitment :
  ?hooks:Process.hooks -> t -> commitment option Lwt.t

(** [last_published_commitment client] gets the last commitment published by the rollup node. *)
val last_published_commitment :
  ?hooks:Process.hooks -> t -> commitment option Lwt.t
