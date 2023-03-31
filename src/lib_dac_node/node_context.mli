(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022-2023 Trili Tech, <contact@trili.tech>                  *)
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
type dac_plugin_module = (module Dac_plugin.T)

(** A [ready_ctx] value contains globally needed information for a running dac
    node. It is available when the DAC plugin has been loaded. Additionally,
    it also contains an instance of [Dac_plugin.hash Data_streamer.t] - a
    component for streaming root hashes, produced during the serialization of
    dac payload. *)
type ready_ctxt = {
  dac_plugin : dac_plugin_module;
  hash_streamer : Dac_plugin.hash Data_streamer.t;
}

(** The status of the dac node. *)
type status = Ready of ready_ctxt | Starting

(** A [t] value contains both the status and the dac node configuration. Its
    fields are available through accessors. *)
type t

(** [init config cctxt dac_node_cctxt] creates a [t] with a status set to
    [Starting] using the given dac node configuration [config],
    tezos node client context [cctxt], and optional client context of
    another dac node [dac_node_cctxt], which can be used for writting
    tests with two dac nodes running the legacy mode. *)
val init :
  Configuration.t ->
  Client_context.full ->
  Dac_node_client.cctxt option ->
  t tzresult Lwt.t

(** Raised by [set_ready] when the status is already [Ready _] *)
exception Status_already_ready

(** [set_ready ctxt dac_plugin_module] updates
    in place the status value to [Ready], and initializes the inner
    [ready_ctxt] value with the given parameters.

    @raise Status_already_ready when the status is already [Ready _] *)
val set_ready : t -> dac_plugin_module -> unit

type error += Node_not_ready

(** [get_ready ctxt] extracts the [ready_ctxt] value from a context [t]. It
    propagates [Node_not_ready] if status is not ready yet. If called multiple
    times, it replaces current values for [ready_ctxt] with new one. *)
val get_ready : t -> ready_ctxt tzresult

(** [get_config ctxt] returns the dac node configuration. *)
val get_config : t -> Configuration.t

(** [get_status ctxt] returns the dac node status. *)
val get_status : t -> status

(** [get_tezos_node_cctxt ctxt] returns the Tezos node's client context. *)
val get_tezos_node_cctxt : t -> Client_context.full

(** [get_dac_plugin ctxt] returns the [Dac_plugin.t] used in the node context.
    Fails with [Node_not_ready] when [Node_context.status] is not [Ready]. *)
val get_dac_plugin : t -> Dac_plugin.t tzresult

(** [get_page_store ctxt] returns the filesystem backed page store used by
    the Dac node to save and load pages. *)
val get_page_store : t -> Page_store.Filesystem.t

(** [get_node_store ctxt access_mode] returns the [Store.Irmin_store.t] with
    [access_mode] used by Dac components. *)
val get_node_store : t -> 'a Store_sigs.mode -> 'a Store.Irmin_store.t

(** [get_committee_members ctxt] returns the Dac committee public key hashes from
    [Configuration.Legacy.dac_members_addresses] or
    [Configuration.Coordinator.dac_members_addresses] *)
val get_committee_members :
  t -> Tezos_crypto.Aggregate_signature.public_key_hash list tzresult

(** [get_coordinator_client ctx] returns the Coordinator client if it 
    is available. *)
val get_coordinator_client : t -> Dac_node_client.cctxt tzresult
