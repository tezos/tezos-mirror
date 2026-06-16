(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2018-2025 Nomadic Labs <contact@nomadic-labs.com> *)
(* SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>              *)
(*                                                                           *)
(*****************************************************************************)

(** Worker that processes the gossipsub P2P output stream. It forwards
    messages to peers and handles connect/disconnect directives from
    the gossipsub worker. *)

(** An opaque handle to the running P2P callback worker. *)
type t

(** [create gs_worker p2p_layer] starts a worker that consumes the output
    stream of [gs_worker] and forwards messages and directives (connect,
    disconnect, kick) through [p2p_layer]. *)
val create :
  Gs_interface.Worker_instance.t ->
  ( Gs_interface.Worker_instance.p2p_message,
    'a,
    Transport_layer_interface.Types.P2P.Metadata.Connection.t )
  Tezos_p2p.P2p.t ->
  (t, tztrace) result Lwt.t

(** [shutdown t] gracefully stops the worker. *)
val shutdown : t -> unit Lwt.t
