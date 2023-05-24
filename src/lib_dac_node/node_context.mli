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

(** [Coordinator] defines a partial [Node_context.t] that is available
    only to [Coordinator] nodes, and functions that can be used to operate
    on such mode-specific node contexts. *)
module Coordinator : sig
  (** The type of a [Coordinator] specific partial [Node_context.t]. *)
  type t = private {
    committee_members : Wallet_account.Coordinator.t list;
        (** The list of [Wallet.Coordinator] values associated with the Data
            Availability Committee members managed by the [Coordinator] node.
        *)
    hash_streamer : Dac_plugin.raw_hash Data_streamer.t;
        (** The [Dac_plugin.hash Data_streamer.t] that the [Coordinator] node
            use to advertise root hashes to [Committee_member] and [Observer]
            nodes. *)
    (* FIXME: https://gitlab.com/tezos/tezos/-/issues/4895
       This could be problematic in case coordinator and member/observer
       use two different plugins that bind different underlying hashes. *)
    certificate_streamers : Certificate_streamers.t;
        (** Certificate streamers to communicate to DAC clients when a
            certificate has been updated with a new signature. *)
  }

  (** [public_keys_opt t] returns the list of public keys associated with the
      data availability committee of [t]. *)
  val public_keys_opt :
    t -> Tezos_crypto.Aggregate_signature.public_key option list

  val committee_members :
    t -> Tezos_crypto.Aggregate_signature.public_key_hash list
end

(** [Committee_member] defines a partial [Node_context.t] that is available
    only to [Committee_member] nodes, and functions that can be used to operate
    on such mode specific partial node contexts. *)
module Committee_member : sig
  (** The type of a [Committee_member] specific partial [Node_context.t]. *)
  type t = private {
    committee_member : Wallet_account.Committee_member.t;
        (**  The [Wallet_account.Committee_member] wallet associated with the
             [commitee_member] managed by the DAC node. *)
    coordinator_cctxt : Dac_node_client.cctxt;
        (** The [Dac_node_client.cctxt] used by the [Committee_member] node to
            send requests to a [Coordinator] node. *)
  }

  (** [secret_key_uri t] returns the secret key URI associated with the
      committee member managed by the [Committee_member] node.  *)
  val secret_key_uri : t -> Client_keys.aggregate_sk_uri
end

(** The type of an [Observer] specific partial [Node_context.t]. *)
module Observer : sig
  type t = private {
    coordinator_cctxt : Dac_node_client.cctxt;
        (** The [Dac_node_client.cctxt] used by the [Observer] node to
        send requests to a [Coordinator] node. *)
    committee_cctxts : Dac_node_client.cctxt list;
        (** The list of [Dac_node_client.cctxt] used by the [Observer] node
        to send requests to each [Committee_member] node respectively. *)
    timeout : int;
        (** Timeout in seconds for fetching a missing page from [Committee_Member]. *)
  }
end

(** [Legacy] defines a partial [Node_context.t] that is available only to
    [Legacy] nodes, and functions that can be used to operate on such
    mode specific partial node contexts. *)
module Legacy : sig
  (** The type of a [Legacy] specific partial [Node_context.t]. *)
  type t = private {
    committee_members : Wallet_account.Legacy.t list;
        (** The list of [Wallet_account.Legacy] values associated with the Data
            Availability Committee members managed by the [Coordinator] node.
        *)
    coordinator_cctxt : Dac_node_client.cctxt option;
        (** An optional [Dac_node_client.cctxt] option. If defined, it
            enables a [Legacy] node to act as if it were an [Observer], using
            the associated [Dac_node_client.cctxt] value to send requests to
            a [Coordinator] node.  *)
    hash_streamer : Dac_plugin.raw_hash Data_streamer.t;
        (** A [Dac_plugin.hash Data_streamer.t] that the [Legacy] node
            use to advertise root hashes to other nodes *)
    committee_member_opt : Wallet_account.Legacy.t option;
        (** The legacy account wallet of the committee member simulated by the
            legacy DAC node, if any. *)
  }

  (** [public_keys_opt t] returns the list of optional public keys associated
      with the committee members of [t]. *)
  val public_keys_opt :
    t -> Tezos_crypto.Aggregate_signature.public_key option list

  (** [secret_key_uris_opt] return the list of optional secret key URIs of the
      committee members of [t]. *)
  val secret_key_uris_opt : t -> Client_keys.aggregate_sk_uri option list

  val committee_members :
    t -> Tezos_crypto.Aggregate_signature.public_key_hash list
end

(** Operating mode specific fraction of a [Node_context.t] *)
type mode = private
  | Coordinator of Coordinator.t
  | Committee_member of Committee_member.t
  | Observer of Observer.t
  | Legacy of Legacy.t

(** A [ready_ctx] value contains globally needed information for a running dac
    node. It is available when the DAC plugin has been loaded. *)
type ready_ctxt = {dac_plugin : dac_plugin_module}

(** The status of the dac node. *)
type status = Ready of ready_ctxt | Starting

(** A [t] value contains both the status and the dac node configuration. Its
    fields are available through accessors. *)
type t

(** [init config cctxt] creates a [t] with a status set to
    [Starting] using the given dac node configuration [config] and
    tezos node client context [cctxt]. *)
val init : Configuration.t -> Client_context.full -> t tzresult Lwt.t

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

(** [get_status ctxt] returns the dac node status. *)
val get_status : t -> status

(** [get_mode node_ctxt] returns the operating mode specific fraction of a
    [Node_context.t]. *)
val get_mode : t -> mode

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
