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
  published_at_level : int option;
}

type simulation_result = {
  state_hash : string;
  status : string;
  output : JSON.t;
  inbox_level : int;
  num_ticks : int;
  insights : string option list;
}

type gc_info = {last_gc_level : int; first_available_level : int}

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

(** [rpc_get client path] issues a GET request for [path]. *)
val rpc_get :
  ?hooks:Process.hooks -> t -> Client.path -> JSON.t Runnable.process

(** [rpc_post client path data] issues a POST request for [path] with [data]. *)
val rpc_post :
  ?hooks:Process.hooks -> t -> Client.path -> JSON.t -> JSON.t Runnable.process

(** [rpc_get_rich ?hooks ?log_output client path parameters] issues a GET
    request for [path] passing [parameters]. *)
val rpc_get_rich :
  ?hooks:Process.hooks ->
  ?log_output:bool ->
  t ->
  Client.path ->
  (string * string) list ->
  JSON.t Runnable.process

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

(** [inspect_durable_state_value ?hooks ?log_output ?block client key]
    gets the corresponding durable PVM state value mapped to [key] for
    the [block] (default ["head"]). *)
val inspect_durable_state_value :
  ?hooks:Process.hooks ->
  ?log_output:bool ->
  ?block:string ->
  t ->
  pvm_kind:string ->
  operation:'a durable_state_operation ->
  key:string ->
  'a Runnable.process

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

(** Same as [outbox_proof_single] without providing the outbox message *)
val outbox_proof :
  ?hooks:Process.hooks ->
  ?expected_error:Base.rex ->
  t ->
  message_index:int ->
  outbox_level:int ->
  outbox_proof option Lwt.t

(** [encode_json_outbox_msg outbox_msg_json] returns the encoding of
    an outbox message. *)
val encode_json_outbox_msg :
  ?hooks:Process.hooks -> t -> JSON.u -> string Runnable.process

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

(** [commitment client hash] gets commitment by its [hash] from the rollup node,
    with its hash and level when the commitment was first published and the
    level it was included. *)
val commitment :
  ?hooks:Process.hooks -> t -> string -> commitment_info option Runnable.process

(** [gc_info client] returns garbage collection information. *)
val gc_info : ?hooks:Process.hooks -> t -> gc_info Runnable.process

(** [get_dal_processed_slots ?block client] returns the slots indices that have
    been marked by the rollup node as confirmed or unconfirmed for block [block]
    (default ["head"]), with their statuses. *)
val get_dal_processed_slots :
  ?hooks:Process.hooks ->
  ?block:string ->
  t ->
  (int * string) list Runnable.process

(** [simulate ?block client ?reveal_pages ?insight_request messages] simulates
    the evaluation of input [messages] for the rollup PVM at [block] (default
    ["head"]). [reveal_pages] can be used to provide data to be used for the
    revelation ticks. [insight_request] can be used to look at a list of keys in
    the PVM state after the simulation. *)
val simulate :
  ?hooks:Process_hooks.t ->
  ?block:string ->
  t ->
  ?reveal_pages:string list ->
  ?insight_requests:
    [`Pvm_state_key of string list | `Durable_storage_key of string list] list ->
  string list ->
  simulation_result Runnable.process

(** [inject client messages] injects the [messages] in the queue the rollup
    node's batcher and returns the list of message hashes injected. *)
val inject :
  ?hooks:Process_hooks.t -> t -> string list -> string list Runnable.process
