(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

include Internal_event.Simple

let section = Events.section @ ["tracer"]

let read_line =
  declare_1
    ~section
    ~name:"call_tracer_read_line"
    ~msg:"Call tracer read a line {line}"
    ~level:Debug
    ("line", Data_encoding.n)

let tracer_input =
  declare_1
    ~section
    ~name:"tracer_input"
    ~msg:"Tracer with input {inputs}"
    ~level:Debug
    ("inputs", Data_encoding.string)

let read_line line = emit read_line line

let tracer_input config = emit tracer_input config
