(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 TriliTech <contact@trili.tech>                         *)
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

type commitment_and_hash = {commitment : commitment; hash : string}

type commitment_info = {
  commitment_and_hash : commitment_and_hash;
  first_published_at_level : int option;
  included_at_level : int option;
}

type slot_header = {level : int; commitment : string; index : int}

type simulation_result = {
  state_hash : string;
  status : string;
  output : JSON.t;
  inbox_level : int;
  num_ticks : int;
}

(** [create ~protocol ?runner ?name ?base_dir node] returns a fresh client
   identified by a specified [name], logging in [color], executing the
   program at [path], storing local information in [base_dir], and
   communicating with the specified [node], using the [runner]. *)
val create :
  protocol:Protocol.t ->
  ?runner:Runner.t ->
  ?name:string ->
  ?base_dir:string ->
  ?color:Log.Color.t ->
  Sc_rollup_node.t ->
  t

(** [sc_rollup_address client] returns the smart contract rollup
   address of the node associated to the [client]. *)
val sc_rollup_address : ?hooks:Process.hooks -> t -> string Runnable.process

(** [rpc_get client path] issues a GET request for [path]. *)
val rpc_get :
  ?hooks:Process.hooks -> t -> Client.path -> JSON.t Runnable.process

(** [rpc_post client path data] issues a POST request for [path] with [data]. *)
val rpc_post :
  ?hooks:Process.hooks -> t -> Client.path -> JSON.t -> JSON.t Runnable.process

(** [rpc_get_rich client path parameters] issues a GET request for [path]
    passing [parameters]. *)
val rpc_get_rich :
  ?hooks:Process.hooks ->
  t ->
  Client.path ->
  (string * string) list ->
  JSON.t Runnable.process

(** [total_ticks ?block client] gets the total number of ticks for the PVM. *)
val total_ticks :
  ?hooks:Process.hooks -> ?block:string -> t -> int Runnable.process

(** [ticks ?block client] gets the number of ticks for the PVM for the [block]
    (default ["head"]). *)
val ticks : ?hooks:Process.hooks -> ?block:string -> t -> int Runnable.process

(** [state_hash ?block client] gets the corresponding PVM state hash for the
    [block] (default ["head"]). *)
val state_hash :
  ?hooks:Process.hooks -> ?block:string -> t -> string Runnable.process

(** [state_current_level ?block client] gets the corresponding PVM state current
    level for the [block] (default ["head"]). *)
val state_current_level :
  ?hooks:Process_hooks.t -> ?block:string -> t -> int Runnable.process

(** [state_value ?block client key] gets the corresponding PVM state value
    mapped to [key] for the [block] (default ["head"]). *)
val state_value :
  ?hooks:Process.hooks ->
  ?block:string ->
  t ->
  key:string ->
  bytes Runnable.process

type 'output_type durable_state_operation =
  | Value : string option durable_state_operation
  | Length : int64 option durable_state_operation
  | Subkeys : string list durable_state_operation

(** [inspect_durable_state_value ?block client key] gets the corresponding durable PVM state value
    mapped to [key] for the [block] (default ["head"]). *)
val inspect_durable_state_value :
  ?hooks:Process.hooks ->
  ?block:string ->
  t ->
  pvm_kind:string ->
  operation:'a durable_state_operation ->
  key:string ->
  'a Runnable.process

(** [status ?block client] gets the corresponding PVM status for the [block]
    (default ["head"]). *)
val status :
  ?hooks:Process.hooks -> ?block:string -> t -> string Runnable.process

(** [outbox ?block outbox_level client] gets the rollup outbox of
   [outbox_level] as known to the [block] (default ["cemented"] which
   is the block corresponding to the last cemented level). *)
val outbox :
  ?hooks:Process.hooks ->
  ?block:string ->
  outbox_level:int ->
  t ->
  JSON.t Runnable.process

type outbox_proof = {commitment_hash : string; proof : string}

(** [outbox_proof_single] asks the rollup node for a proof that an
    output of a given [message_index] is available in the outbox at a
    given [outbox_level] as a latent call to [destination]'s
    [entrypoint] with the given [parameters]. *)
val outbox_proof_single :
  ?hooks:Process.hooks ->
  ?expected_error:Base.rex ->
  ?entrypoint:string ->
  ?parameters_ty:string ->
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
  parameters_ty : string option;
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

(** [encode_batch batch] returns the encoding of a [batch] of output
   transactions. *)
val encode_batch :
  ?hooks:Process.hooks ->
  ?expected_error:Base.rex ->
  t ->
  transaction list ->
  string option Lwt.t

(** [commitment_from_json] parses a commitment from its JSON representation. *)
val commitment_from_json : JSON.t -> commitment option

(** [commitment_info_from_json] parses a commitment, its hash and
    the levels when the commitment was first published (if any) and included,
    from the JSON representation. *)
val commitment_info_from_json : JSON.t -> commitment_info option

(** [last_stored_commitment client] gets the last commitment with its hash
    stored by the rollup node. *)
val last_stored_commitment :
  ?hooks:Process.hooks -> t -> commitment_and_hash option Runnable.process

(** [last_published_commitment client] gets the last commitment published by the
    rollup node, with its hash and level when the commitment was first published
    and the level it was included. *)
val last_published_commitment :
  ?hooks:Process.hooks -> t -> commitment_info option Runnable.process

(** [dal_slot_headers ?block client] returns the dal slot headers of the
    [block] (default ["head"]). *)
val dal_slot_headers :
  ?hooks:Process.hooks ->
  ?block:string ->
  t ->
  slot_header list Runnable.process

(** [get_dal_processed_slots ?block client] returns the slots indices that have
    been marked by the rollup node as confirmed or unconfirmed for block [block]
    (default ["head"]), with their statuses. *)
val get_dal_processed_slots :
  ?hooks:Process.hooks ->
  ?block:string ->
  t ->
  (int * string) list Runnable.process

(** [simulate ?block client ?reveal_pages messages] simulates the evaluation of
    input [messages] for the rollup PVM at [block] (default
    ["head"]). [reveal_pages] can be used to provide data to be used for the
    revelation ticks. *)
val simulate :
  ?hooks:Process_hooks.t ->
  ?block:string ->
  t ->
  ?reveal_pages:string list ->
  string list ->
  simulation_result Runnable.process

(** [inject client messages] injects the [messages] in the queue the rollup
    node's batcher and returns the list of message hashes injected. *)
val inject :
  ?hooks:Process_hooks.t -> t -> string list -> string list Runnable.process

(** [batcher_queue client] returns the queue of messages, as pairs of message
    hash and binary message, in the batcher. *)
val batcher_queue :
  ?hooks:Process_hooks.t -> t -> (string * string) list Runnable.process

(** [get_batcher_msg client hash] fetches the message whose hash is [hash] from
    the queue. It returns the message together with the full JSON response
    including the status. *)
val get_batcher_msg :
  ?hooks:Process_hooks.t -> t -> string -> (string * JSON.t) Runnable.process

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
