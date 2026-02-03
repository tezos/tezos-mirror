(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

type state = {
  migration_level : int;
  node : Tezt_tezos.Node.t;
  network :
    [ `Ghostnet
    | `Mainnet
    | `Nextnet of string
    | `Sandbox
    | `Seoulnet
    | `Shadownet
    | `Tallinnnet
    | `Weeklynet of string ];
  next_protocol : Tezt_tezos.Protocol.t;
  artifacts_dir_path : string;
  mutable protocol_parameters : JSON.t option;
  mutable next_protocol_parameters : JSON.t option;
}

let init_state migration_level node network next_protocol artifacts_dir_path =
  {
    migration_level;
    node;
    network;
    next_protocol;
    artifacts_dir_path;
    protocol_parameters = None;
    next_protocol_parameters = None;
  }

let registered = Hashtbl.create 10

let register ~(title : string) (body : state -> int -> state Lwt.t) =
  Hashtbl.add registered title body

let get title = Hashtbl.find_opt registered title
