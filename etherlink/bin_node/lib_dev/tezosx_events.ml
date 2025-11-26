(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

include Internal_event.Simple

let section = Events.section @ ["tezosx"]

let runtime_activated =
  declare_1
    ~section
    ~name:"runtime_activated"
    ~msg:"the runtime {runtime} is activated"
    ~level:Notice
    ("runtime", Tezosx.runtime_encoding)

let runtime_activated runtime = emit runtime_activated runtime
