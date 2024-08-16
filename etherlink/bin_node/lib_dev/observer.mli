(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** [main ?kernel_path ~rollup_node_endpoint ~evm_node_endpoint
    ~data_dir ~config] starts the main event-loop of the Observer,
    consuming the blueprints received from [evm_node_endpoint]. *)
val main :
  ?kernel_path:string ->
  data_dir:string ->
  config:Configuration.t ->
  unit ->
  unit tzresult Lwt.t

module Make (Ctxt : sig
  val evm_node_endpoint : Uri.t

  val keep_alive : bool

  val smart_rollup_address : Tezos_crypto.Hashed.Smart_rollup_address.t
end) : Services_backend_sig.S
