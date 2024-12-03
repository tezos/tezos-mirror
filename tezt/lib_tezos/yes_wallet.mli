(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

type t

val create : ?runner:Runner.t -> ?path:string -> ?name:string -> unit -> t

(** Initialize a client wallet
    {[
        yes_wallet.exe create \
            from context <DATA_DIR> \
            in <BASE_DIR> \
            --network network \
            --aliases aliases_filename \
            --force \
            --active-bakers-only
    ]}

    [<DATA_DIR>] is defined by [node] and [<BASE_DIR>] from [client]
*)
val create_from_context :
  ?aliases:(string * string * string) list ->
  node:Node.t ->
  client:Client.t ->
  network:string ->
  t ->
  unit Lwt.t
