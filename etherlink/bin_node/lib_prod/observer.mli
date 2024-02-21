(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** [main ctxt ~evm_node_endpoint] starts the main event-loop of the Observer,
    consuming the blueprints received from [evm_node_endpoint]. *)
val main : Evm_context.t -> evm_node_endpoint:Uri.t -> unit tzresult Lwt.t

module Make (Ctxt : sig
  val ctxt : Evm_context.t

  val evm_node_endpoint : Uri.t
end) : Services_backend_sig.S
