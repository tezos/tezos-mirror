(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** [main ~config block] starts a REPL allowing the user to inspect the content
    of the durable storage at the specified [block], as stored in
    [config.data_dir]. *)
val main :
  config:Configuration.t ->
  Ethereum_types.Block_parameter.extended ->
  unit tzresult Lwt.t

(** [cat ~config block pp path] prints to the standard output the value stored
    under [path] for the specified [block], using the pretty-printer kind [pp].
    *)
val cat :
  config:Configuration.t ->
  Ethereum_types.Block_parameter.extended ->
  Tezos_layer2_shell.Pp.t ->
  string ->
  unit tzresult Lwt.t

(** [ls ~config block path] lists the child directories under [path] for the
    specified [block]. *)
val ls :
  config:Configuration.t ->
  Ethereum_types.Block_parameter.extended ->
  string ->
  unit tzresult Lwt.t
