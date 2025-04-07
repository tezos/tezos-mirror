(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

type alias = {alias : string; address : string; public_key : string}

type aliases = File of string | List of alias list

type t

val create : ?runner:Runner.t -> ?path:string -> ?name:string -> unit -> t

(** Initialize a client wallet
    {[
        yes_wallet.exe create \
            from context <DATA_DIR> \
            in <BASE_DIR> \
            --network network \
            --aliases <ALIASES_FILE_PATH> \
            --force \
            --active-bakers-only
    ]}

    [<DATA_DIR>] is defined by [node] and [<BASE_DIR>] from [client]

    Returns [<ALIASES_FILE_PATH>], which is either the file provided
    via [~aliases:(File "filename")] or the path where aliases passed
    as [~aliases:(List aliases)] have been dumped.
*)
val create_from_context :
  ?aliases:aliases ->
  node:Node.t ->
  client:Client.t ->
  network:string ->
  t ->
  string Lwt.t

(** Initialize a client wallet
    {[
        yes_wallet.exe convert \
            wallet <BASE_DIR> \
            inplace \
            --force \
    ]}

    [<BASE_DIR>] is defined by [client]
*)
val convert_wallet_inplace : client:Client.t -> t -> unit Lwt.t
