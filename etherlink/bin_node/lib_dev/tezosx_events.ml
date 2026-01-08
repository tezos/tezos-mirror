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

let fail =
  declare_1
    ~section
    ~name:"list_runtime_failed"
    ~msg:"runtime flags read failed with error {error}"
    ~level:Warning
    ~pp1:Error_monad.pp_print_trace
    ("error", Events.trace_encoding)

let runtime_activated runtime = emit runtime_activated runtime

let list_runtime_failed err = emit fail err
