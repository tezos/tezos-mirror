(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Internal_event.Simple

let section = ["floodgate"]

let is_ready =
  declare_0 ~section ~name:"start" ~msg:"Floodgate is ready" ~level:Notice ()

let is_ready () = emit is_ready ()
