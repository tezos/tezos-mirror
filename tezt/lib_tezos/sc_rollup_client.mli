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
  number_of_ticks : int;
}

type slot_header = {level : int; commitment : string; index : int}

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

(** [total_ticks ?block client] gets the total number of ticks for the PVM. *)
val total_ticks : ?hooks:Process.hooks -> ?block:string -> t -> int Lwt.t

(** [ticks ?block client] gets the number of ticks for the PVM for the [block]
    (default ["head"]). *)
val ticks : ?hooks:Process.hooks -> ?block:string -> t -> int Lwt.t

(** [state_hash ?block client] gets the corresponding PVM state hash for the
    [block] (default ["head"]). *)
val state_hash : ?hooks:Process.hooks -> ?block:string -> t -> string Lwt.t

(** [state_value ?block client key] gets the corresponding PVM state value
    mapped to [key] for the [block] (default ["head"]). *)
val state_value :
  ?hooks:Process.hooks -> ?block:string -> t -> key:string -> bytes Lwt.t

(** [status ?block client] gets the corresponding PVM status for the [block]
    (default ["head"]). *)
val status : ?hooks:Process.hooks -> ?block:string -> t -> string Lwt.t

(** [outbox ?block client] gets the rollup outbox for the [block] (default
    ["cemented"] which is the block corresponding to the last cemented
    level). *)
val outbox : ?hooks:Process.hooks -> ?block:string -> t -> string Lwt.t

type outbox_proof = {commitment_hash : string; proof : string}

(** [outbox_proof_single] asks the rollup node for a proof that an
    output of a given [message_index] is available in the outbox at a
    given [outbox_level] as a latent call to [destination]'s
    [entrypoint] with the given [parameters]. *)
val outbox_proof_single :
  ?hooks:Process.hooks ->
  ?expected_error:Base.rex ->
  ?entrypoint:string ->
  t ->
  message_index:int ->
  outbox_level:int ->
  destination:string ->
  parameters:string ->
  outbox_proof option Lwt.t

type transaction = {
  destination : string;
  entrypoint : string option;
  parameters : string;
}

(** Same as [outbox_proof_single] except that the claim is about a batch
    of output transactions. *)
val outbox_proof_batch :
  ?hooks:Process.hooks ->
  ?expected_error:Base.rex ->
  t ->
  message_index:int ->
  outbox_level:int ->
  transaction list ->
  outbox_proof option Lwt.t

(** [commitment_from_json] parses a commitment from its JSON representation. *)
val commitment_from_json : JSON.t -> commitment option

(** [commitment_with_hash_and_level_from_json] parses a commitment, its hash
    and the level when the commitment was first published (if any), from the
    JSON representation. *)
val commitment_with_hash_and_level_from_json :
  JSON.t -> (string * commitment * int option) option

(** [last_stored_commitment client] gets the last commitment with its hash
    stored by the rollup node. *)
val last_stored_commitment :
  ?hooks:Process.hooks -> t -> (string * commitment * int option) option Lwt.t

(** [last_published_commitment client] gets the last commitment published by the rollup node,
with its hash and level when the commitment was first published. *)
val last_published_commitment :
  ?hooks:Process.hooks -> t -> (string * commitment * int option) option Lwt.t

(** [dal_slot_subscriptions ?block client] gets the slots to which the rollup
    node is subscribed to, for the [block] (default ["head"]). *)
val dal_slot_subscriptions :
  ?hooks:Process.hooks -> ?block:string -> t -> int list Lwt.t

(** [dal_slot_headers ?block client] returns the dal slot headers of the
    [block] (default ["head"]). *)
val dal_slot_headers :
  ?hooks:Process.hooks -> ?block:string -> t -> slot_header list Lwt.t

(** [dal_downloaded_slots ?block client] returns the slots downloaded after processing
    the [block] (default ["head"]). *)
val dal_downloaded_slots :
  ?hooks:Process.hooks ->
  ?block:string ->
  t ->
  (int * string option list) list Lwt.t

(** [generate_keys ~alias client] generates new unencrypted keys for [alias]. *)
val generate_keys :
  ?hooks:Process.hooks -> ?force:bool -> alias:string -> t -> unit Lwt.t

(** [list_keys client] returns the known aliases with their public key hash from
    client.

    Fails if the format isn't in the form `<alias>: <public key hash>`. *)
val list_keys : ?hooks:Process.hooks -> t -> (string * string) list Lwt.t

(** [show_address ~alias client] returns the BLS account associated with [alias].

    Fails if the output from the client isn't in the expected format (see
    {!Client.show_address}). *)
val show_address :
  ?hooks:Process.hooks -> alias:string -> t -> Account.aggregate_key Lwt.t

(** [import_secret_key account] imports [account.alias] as alias to
    [account.secret_key] into the client. *)
val import_secret_key :
  ?hooks:Process.hooks ->
  ?force:bool ->
  Account.aggregate_key ->
  t ->
  unit Lwt.t
